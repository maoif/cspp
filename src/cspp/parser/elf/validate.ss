(library (cspp parser elf validate)
  (export make-elf-issue elf-issue? elf-issue-offset elf-issue-message
          elf-power-of-two? elf-range-valid?)
  (import (cspp chez)
          (cspp utils))

  (define exact-natural?
    (lambda (value) (and (natural? value) (exact? value))))

  (define-record-type ($elf-issue $make-elf-issue $elf-issue?)
    (fields (immutable offset $elf-issue-offset)
            (immutable message $elf-issue-message)))

  #|proc:make-elf-issue
  The `make-elf-issue` procedure creates an ELF issue at exact nonnegative `offset` with string
  `message`.
  |#
  (define make-elf-issue
    (lambda (offset message)
      (pcheck ([exact-natural? offset] [string? message])
              ($make-elf-issue offset message))))

  #|proc:elf-issue?
  The `elf-issue?` procedure returns whether `object` is an ELF issue record.
  The `object` parameter is the object to test.
  |#
  (define elf-issue?
    (lambda (object)
      (pcheck () ($elf-issue? object))))

  #|proc:elf-issue-offset
  The `elf-issue-offset` procedure returns the byte offset stored in issue `record`.
  |#
  (define elf-issue-offset
    (lambda (record)
      (pcheck ([$elf-issue? record])
              ($elf-issue-offset record))))

  #|proc:elf-issue-message
  The `elf-issue-message` procedure returns the diagnostic string stored in issue `record`.
  |#
  (define elf-issue-message
    (lambda (record)
      (pcheck ([$elf-issue? record])
              ($elf-issue-message record))))

  #|proc:elf-power-of-two?
  The `elf-power-of-two?` procedure returns whether exact nonnegative integer `value` is zero
  or a power of two.
  |#
  (define elf-power-of-two?
    (lambda (value)
      (pcheck ([exact-natural? value])
              (or (= value 0)
                  (and (positive? value) (= 0 (logand value (- value 1))))))))

  #|proc:elf-range-valid?
  The `elf-range-valid?` procedure returns whether byte range `offset` and `size` fits within
  input `length`. All three arguments must be exact nonnegative integers.
  |#
  (define elf-range-valid?
    (lambda (offset size length)
      (pcheck ([exact-natural? offset size length])
              (and (<= offset length) (<= size (- length offset))))))
  )
