#!chezscheme
(import (cspp))

(define-record-type (dashboard-lines make-dashboard-lines dashboard-lines?)
  (fields (immutable lines dashboard-lines-lines)))

(rich-register-renderer! dashboard-lines? dashboard-lines-lines)

(define section
  (lambda (console title)
    (rich-println console)
    (let ([rule (rich-rule :title title :style 'double)])
      (rich-print console (rich-style 'bold #x66ddff) rule (reset-style) "\n"))))

(define print-lines
  (lambda (console lines)
    (rich-print console (make-dashboard-lines lines))
    (rich-println console)))

(define run-prompt-demo
  (lambda (console)
    (call-with-port (open-input-string "us-east\nblue\nrobot-token\nn\n")
      (lambda (input)
        (call-with-port (open-output-string)
          (lambda (output)
            (let* ([prompt-console (rich-console :input-port input :output-port output
                                                :color-system 'none)]
                   [region (rich-prompt prompt-console "Region" "local"
                                        '("us-east" "eu-west" "local"))]
                   [ring (rich-prompt prompt-console "Ring" "green" '("blue" "green"))]
                   [secret (rich-password prompt-console "Robot token")]
                   [approved? (rich-confirm prompt-console "Open traffic?")]
                   [table (rich-table :title "Automated Prompt Session" :box 'square
                                      :columns ("Field" "Value"))])
              (rich-table-add-row! table "region" region)
              (rich-table-add-row! table "ring" ring)
              (rich-table-add-row! table "token" secret)
              (rich-table-add-row! table "approved" (if approved? "true" "false"))
              (rich-table-add-row! table "prompt transcript" (get-output-string output))
              (rich-print console table "\n"))))))))

(define run-progress-demo
  (lambda (console)
    (let ([deploy (make-rich-progress)])
      (let ([assets (rich-progress-add-task! deploy "资源 assets" 8)]
            [migrate (rich-progress-add-task! deploy "迁移 migrations" 5)]
            [workers (rich-progress-add-task! deploy "workers" #f)])
        (let ([live (make-rich-live deploy console 0 #f)])
          (let loop ([i 0])
            (when (<= i 8)
              (rich-progress-update! deploy assets i)
              (rich-progress-update! deploy migrate (min i 5))
              (rich-progress-advance! deploy workers 1)
              (rich-live-refresh! live)
              (milisleep 500)
              (loop (+ i 1))))
          (rich-progress-complete! deploy assets)
          (rich-progress-complete! deploy migrate)
          (rich-live-refresh! live)
          (rich-live-stop! live)
          (rich-println console "\n"))))))

(define console (make-rich-console))

(define dashboard-theme
  (rich-theme
    :good (rich-style 'bold 'green)
    :busy (rich-style 'bold 'yellow)
    :bad (rich-style 'bold 'red)
    :muted (rich-style 'dim)
    :accent (rich-style 'bold #x00b7ff)))
(rich-console-theme-set! console dashboard-theme)

(rich-print console
            (rich-theme-ref dashboard-theme 'accent)
            "Operations Dashboard Example"
            (reset-style)
            "\n")
(rich-print console
            (rich-style 'dim)
            "A second complete Rich script with different data and layout choices."
            (reset-style)
            "\n")

(section console "Styled Signals")
(define status-text (rich-text "api " 'muted))
(rich-text-append! status-text "healthy " 'good)
(rich-text-append! status-text "queue " 'muted)
(rich-text-append! status-text "busy " 'busy)
(rich-text-append! status-text "cache " 'muted)
(rich-text-append! status-text "warm" (rich-style 'underline 'cyan))
(rich-print console status-text "\n")
(rich-print console
            (rich-style 'bold 'white '(bg #x224466))
            "region: us-east"
            (reset-style)
            " "
            (rich-style 'italic #xffcc33)
            "canary ring active"
            (reset-style)
            "\n")

(section console "Wrapping And Cropping")
(let* ([story "the deployment note wraps across several short lines while preserving magenta styling"]
       [wrapped (rich-segment-wrap (list (rich-segment story (rich-style 'magenta)))
                                   (rich-console-width console))]
       [cropped (rich-segment-crop
                 (list (rich-segment "incident-2026-05-02/very-long-service-name"
                                     (rich-style 'yellow)))
                 (max 1 (quotient (rich-console-width console) 2)))])
  (print-lines console wrapped)
  (print-lines console (list cropped))
  (rich-print console "visible width check: "
              (rich-segments-width (list (rich-segment "deploy")
                                         (rich-segment "\033[1m" #f #t)))
              "\n"))

(section console "Unicode Cells / 中文")
;; CJK, combining accents, and a simple emoji align in scalar and grapheme terminals.
;; Multi-code-point emoji are shown outside borders because terminal shaping differs.
(let ([table (rich-table :box 'rounded :columns ("Text" "Cells"))])
  (for-each
   (lambda (text)
     (rich-table-add-row! table
                         (rich-text text (rich-style 'cyan))
                         (number->string (rich-segment-width (rich-segment text)))))
   '("中文" "café" "かな" "한글" "🚀"))
  (rich-print console table "\n"))
(rich-print console "Emoji sequences: 👩🏽‍💻  🇨🇳  ❤️\n")
(print-lines console
             (rich-segment-wrap
              (list (rich-segment "中文 café 🚀 编程" (rich-style 'magenta)))
              (rich-console-width console)))
(rich-print console "Cropped: "
            (make-dashboard-lines (list (rich-segment-crop
                                         (list (rich-segment "中文🚀 café"))
                                         (max 1 (quotient (rich-console-width console) 2)))))
            "\n")
(rich-println console "Aligned:")
(rich-print console (rich-align :body "中文 é 🚀" :align 'right) "\n")

(section console "Rules, Alignment, Padding")
(define deploy-rule
  (rich-rule :title "release train" :style 'rounded))
(define left (rich-align :body "left: logs" :align 'left))
(define center (rich-align :body "center: service map" :align 'center))
(define right (rich-align :body "right: alerts" :align 'right))
(define spacer
  (rich-padding
    :body (rich-text "padded command output" (rich-style 'green))
    :top 1
    :right 6
    :bottom 1
    :left 6))
(rich-print console deploy-rule "\n" left "\n" center "\n" right "\n" spacer "\n")

(section console "Panels, Boxes, Columns")
(define api-panel
  (rich-panel
    :title "api"
    :body (rich-text "p95 82ms\nerrors 0.03%" (rich-style 'green))
    :box 'ascii))
(define queue-panel
  (rich-panel
    :title "queue"
    :body (rich-text "depth 142\nworkers 18" (rich-style 'yellow))
    :box 'square))
(define cache-panel
  (rich-panel
    :title "cache"
    :body (rich-text "hit 97%\nmiss 3%" (rich-style 'cyan))
    :box 'rounded))
(define deploy-panel
  (rich-panel
    :title "deploy"
    :body (rich-text "ring blue\nversion 42" (rich-style 'bold 'magenta))
    :box 'double))
(define alert-panel
  (rich-panel
    :title "alerts"
    :body (rich-text "none active" (rich-style 'bold 'green))
    :box 'heavy))
(define service-cards
  (rich-columns
    :items (list api-panel queue-panel cache-panel deploy-panel alert-panel)
    :gap 2))
(rich-print console service-cards "\n")

(section console "Tables")
(define service-table
  (rich-table
    :title "Service Health"
    :caption "Double box, styled cells, and multiline table values"
    :box 'double
    :show-header? #t
    :show-lines? #t
    :columns ("Service" "State" "Detail")
    :rows (("api" (rich-text "green" 'good) "p95 82ms")
           ("queue" (rich-text "yellow" 'busy) "142 jobs\n18 workers")
           ("edge" (rich-text "green" 'good) "2 regions"))))
(rich-print console service-table "\n")

(section console "Tree, Layout, Pretty")
(define topology
  (rich-tree :label (rich-text "platform" (rich-style 'bold 'blue))))
(let ([edge (rich-tree-add! topology "edge")])
  (rich-tree-add! edge "cdn")
  (rich-tree-add! edge "waf"))
(let ([core (rich-tree-add! topology (rich-text "core" (rich-style 'cyan)))])
  (rich-tree-add! core "api")
  (rich-tree-add! core "queue")
  (rich-tree-add! core "database\nreplica"))
(rich-print console topology "\n")
(define horizontal
  (rich-layout
    :direction 'row
    :items ((rich-text "ingest" (rich-style 'green))
            (rich-text "process" (rich-style 'yellow))
            (rich-text "serve" (rich-style 'cyan)))))
(define vertical
  (rich-layout
    :direction 'column
    :items ("pipeline" horizontal "observability")))
(define layout-panel
  (rich-panel
    :title "layout"
    :body vertical
    :box 'rounded))
(define pretty-panel
  (rich-panel
    :title "pretty datum"
    :body '(dashboard (region . us-east) (ring . blue) #(api queue edge))
    :box 'square))
(rich-print console layout-panel "\n" pretty-panel "\n")

(section console "Live Status And Progress")
(let ([now 0])
  (parameterize ([rich-current-time (lambda () now)])
    (let ([status (make-rich-status "Sampling telemetry"
                                    console
                                    (list "◐" "◓" "◑" "◒")
                                    1)])
      (let ([live (make-rich-live status console 0 #f)])
        (rich-live-refresh! live)
        (milisleep 500)
        (set! now 1)
        (rich-live-refresh! live)
        (milisleep 500)
        (set! now 2)
        (rich-live-refresh! live)
        (milisleep 500)
        (set! now 3)
        (rich-live-refresh! live)
        (rich-println console)))))
(run-progress-demo console)

(section console "Prompts")
(run-prompt-demo console)

(section console "Exports")
(let ([plain (rich-export-text service-table)]
      [ansi (rich-export-ansi (rich-text "dashboard ansi export" (rich-style 'bold 'green)))])
  (define export-table
    (rich-table
      :box 'rounded
      :columns ("Capability" "Value")
      :rows (("text export length" (number->string (string-length plain)))
             ("ansi export" (rich-text (rich-strip-ansi ansi) (rich-style 'bold 'green)))
             ("registered renderer" (if (rich-renderable? service-table) "yes" "no"))
             ("color system" (if (rich-color-system? 'standard) "standard" "unknown")))))
  (rich-print console export-table "\n"))
