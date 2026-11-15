# Command-line parsing

`(cspp cli)` is a library-first command-line parser. Programs construct a
command tree, configure options and subcommands, and call
`run-cli-command!`. Successful parsing normally produces no output. Parse
failures signal Scheme errors. Built-in help prints to `current-output-port`
and returns without running command actions.

## Scope

Supported features:

- command and option records with procedural setters
- reusable option groups
- built-in value parsers
- pure boolean flags
- `=`-only named option values
- positional and sink positional arguments
- nested subcommands
- command exec callbacks
- option aliases for lookup
- option relations: requires, conflicts, and overrides
- built-in help output
- macro constructors for commands, options, option groups, and enums

Unsupported features:

- environment variables
- config files
- response files
- shell completion
- edit-distance suggestions
- style/color rendering
- prefix option values such as `-Ipath`
- consume-after options
- short option grouping such as `-abc`
- executable-name dispatch for multi-personality commands

Some command and option fields store metadata without affecting parsing or rendering.
For example, `multi-personality?` is accepted only on the top command and does not
dispatch on executable name.

## Data Model

### Commands

A command is created with:

```scheme
(make-command 'prog)
```

The command name is stored as a string because argv subcommand matching compares
against strings. The constructor accepts a symbol whose string form is nonempty
and does not start with `-`.

Commands contain:

- display metadata: help, overview, version, author, copyright, about, style,
  and category
- an optional exec procedure
- ordered option and subcommand builders
- behavior flags such as `maybe-no-subcommand?`
- accepted metadata flags such as `multi-personality?`, `flatten-help?`,
  `trace?`, and `quit?`

`command-options-add!` appends option objects. `command-subcommands-add!`
appends command objects and rejects adding a command as its own subcommand.

### Options

An option is created with:

```scheme
(make-option 'verbose)
```

Options default to pure boolean flag behavior:

- short spelling inferred from the first character, e.g. `-v`
- long spelling inferred from the full name, e.g. `--verbose`
- occurrence count `*`
- value count `0`
- parser `parser-bool`
- default value `#f`

Changing an option from `parser-bool` to a non-bool parser automatically changes
`value-number` from `0` to `1`, so:

```scheme
(define output (make-option 'output))
(option-value-parser-set! output parser-string)
```

accepts `--output=file` without also setting `value-number`.

`option-name-set!` updates short and long spellings when they match the spellings
inferred from the option's name before the call. Explicit spellings are preserved.

Short names are one-character strings and cannot be `-` or `=`. Long names are
strings that are nonempty, do not start with `-`, and do not contain `=`.

### Option Groups

Option groups are reusable collections of options:

```scheme
(define common
  (make-option-group
   (cli-option verbose :short #\v)
   (cli-option config :value-parser string)))
```

Procedurally, use `make-option-group`, `option-group-add!`, and
`option-group-options`. The macro API accepts option groups anywhere an option
item is accepted and expands the group's member options into the command.

## Parse Semantics

### Named Options

Named option values are accepted only with `=`:

```text
--output=file
-o=file
```

The following forms are rejected for value-taking options:

```text
--output file
-o file
```

Pure flags accept no value:

```text
--verbose
-v
```

The following forms are rejected for pure flags:

```text
--verbose=true
-v=true
```

Named options are command-local. A root option must appear before the
subcommand. A subcommand option must appear after that subcommand name.

### Occurrence Counts

`option-number` controls how many times an option may occur:

- `1`: required exactly once
- `?`: optional, at most once
- `+`: required at least once
- `*`: optional, repeatable

Repeated scalar options keep the last value. Repeated list-valued options append
their parsed lists.

### Value Counts

`option-value-number` controls how many values one option occurrence consumes:

- `0`: no value; parser receives `#f`
- `1`: one nonempty value
- `?`: optional value; missing or empty uses the default value
- `+`: one or more separator-delimited values
- `*`: zero or more separator-delimited values

Multi-value payloads are split with `option-value-seperator`, which defaults to
comma. Empty elements are errors.

### Positionals

Positionals are options with `option-positional?` set to `#t`. Making an option
positional disables its short and long spellings and sets it to required
single-value string parsing.

Only leaf commands consume positionals. A command cannot have both positionals
and subcommands.

A sink positional consumes all remaining positional argv entries and must be the
last positional option. The sink stores a list of parsed values. Sinks require at
least one argument by default. To permit zero arguments, configure the occurrence
count after `:sink`:

```scheme
[args :positional :sink :number * :default '()]
```

Fixed positional arguments before the sink remain required.

`--` is accepted only in leaf commands. It switches remaining argv entries to
positional-only mode and is not included in the positional values. Later `--`
arguments are preserved literally. If positional parsing starts before the
separator, its first `--` is also consumed and subsequent ones are preserved.

### Subcommands

At a command with subcommands, the next non-option argv entry selects a
subcommand by exact command-name match.

Before descending into a subcommand, the current command is finalized:

- defaults are filled
- required options are checked
- overrides are applied
- conflicts and requires are checked

If no subcommand is provided:

- the command may run if it has an exec procedure
- the command may run if `maybe-no-subcommand?` is true
- otherwise parsing raises an error

### Exec Callbacks

`command-exec-set!` installs an action:

```scheme
(command-exec-set! cmd
  (lambda (ref)
    (list (ref 'verbose) (ref 'output))))
```

The callback receives a lookup procedure. The lookup key is an option name or
alias symbol. Missing options have their finalized default values. A lookup for
an option outside the command prefix visible to that exec callback raises an
error.

Exec callbacks run only after the selected command path is known and validated.
They run in command path order. The return value of `run-cli-command!` is the
last non-void exec result. If no exec callback runs, `run-cli-command!` returns
`(void)`.

Selected-path validation happens before exec callbacks. This prevents command
actions from running when the selected command path has ambiguous option names
or aliases.

## Validation Rules

Command tree validation rejects:

- cycles
- duplicate sibling subcommand names
- `multi-personality?` on non-top commands

Command-local option validation rejects:

- duplicate option names
- duplicate option aliases
- duplicate short spellings
- duplicate long spellings
- more than one sink positional
- sink positionals that are not last
- commands that have both positionals and subcommands

Selected-path validation rejects duplicate option names or aliases across the
commands that were actually selected. This keeps the exec lookup procedure
unambiguous.

## Option Relations

Relations store option names as symbols. Setters accept a symbol, an option, or
a list of symbols/options.

`requires` means another option must be present:

```scheme
(option-requires-set! opt-a '(opt-b))
```

`conflicts` means another option must not be present:

```scheme
(option-conflicts-set! opt-a '(opt-b))
```

`overrides` removes another option from the parse state before conflict and
requires checks:

```scheme
(option-overrides-set! opt-c '(opt-a opt-b))
```

Overrides are applied before conflicts and requires. Therefore, if `c`
overrides `a` and `b`, the input `-a -b -c` finalizes as only `c` present.

## Built-In Parsers

All parser procedures have the shape:

```scheme
(lambda (cmd opt raw) ...)
```

Available parsers:

- `parser-bool`: accepts `#f` raw value and returns `#t`
- `parser-natural`: exact nonnegative integer
- `parser-integer`: exact integer
- `parser-fixnum`: fixnum
- `parser-flonum`: real number converted to inexact
- `parser-string`: raw string identity
- `parser-enum`: enum parser generated from symbolic specs

Enum specs may be symbols or lists beginning with a symbol:

```scheme
(parser-enum 'red '[green :value 2] '[blue :value 3 :help "Blue"])
```

`:help` is accepted as metadata in enum specs. It does not affect parsing.

## Built-In Help

The built-in help spellings are:

- `-h`
- `--help`
- `--help-all`
- `--help-commands`

If the current command explicitly defines an option using the same spelling,
that option wins and built-in help is not triggered.

`--help` prints help for the current command and excludes hidden options.
`--help-all` includes hidden options. `--help-commands` prints recursive command
paths from the top command.

Help output includes:

- top command overview, version, and author when available
- usage
- positional arguments
- non-positional options
- subcommands
- longer `:about` text after the options and subcommands, when provided

Options and commands are grouped by category. Empty categories render as
`OPTIONS` or `COMMANDS`; nonempty categories render using the category string.
Category groups preserve first-seen declaration order.

## Macro API

The macro API expands directly to procedural construction. It does not maintain
separate runtime data structures.

### `cli-option`

```scheme
(cli-option output
  :short #\o
  :value-parser string
  :default "a.out")
```

Parser identifiers are resolved as:

- `bool` -> `parser-bool`
- `natural` -> `parser-natural`
- `integer` -> `parser-integer`
- `fixnum` -> `parser-fixnum`
- `flonum` -> `parser-flonum`
- `string` -> `parser-string`

Other parser expressions are used as written.

Supported option keywords:

- `:help`
- `:default`
- `:number`
- `:value-number`
- `:value-name`
- `:value-parser`
- `:value-seperator`
- `:positional`
- `:sink`
- `:short`
- `:long`
- `:no-short`
- `:no-long`
- `:callback`
- `:alias`
- `:hidden`
- `:category`
- `:no-grouping`
- `:conflicts`
- `:overrides`
- `:requires`

Unknown keywords are syntax errors.

### `cli-option-group`

```scheme
(cli-option-group
 [verbose :short #\v]
 [output :value-parser string])
```

Items can be inline option specs, option identifiers, or option-group
identifiers.

### `cli-enum`

```scheme
(define mode-parser
  (cli-enum [fast :value fast]
            [safe :value safe]))
```

The macro quotes enum specs and passes them to `parser-enum`.

### `cli-command`

```scheme
(define cmd
  (cli-command prog
    :overview "Program"
    :exec main
    :options
    [verbose :short #\v]
    common-options
    :subcommands
    [run
     :help "Run"
     :exec run-main
     :options
     [input :positional]]))
```

Supported command keywords:

- `:overview`
- `:version`
- `:author`
- `:copyright`
- `:about`
- `:style`
- `:category`
- `:exec`
- `:help`
- `:trace`
- `:quit`
- `:flatten-help`
- `:multi-personality`
- `:maybe-no-subcommand`
- `:options`
- `:subcommands`

Unknown command keywords are syntax errors. Unsupported features have no special macro forms.

## Testing

`tests/cli.ss` is executable behavior documentation. It covers:

- procedural option parsing
- pure flags and `=`-only values
- built-in parsers
- repeated values and sink positionals
- option relations
- nested subcommands
- selected-path validation
- help output, hidden options, command lists, and categories
- macro construction
- malformed macro inputs

Focused verification:

```bash
cd tests
make test cli.ss
```

When running through the Makefile, normal successful tests should not emit
stdout/stderr from the Scheme code. The Makefile itself prints compile and run
progress lines.
