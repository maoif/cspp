(library (cspp system filesystem)
  (export
          ;; filesystem capacity records
          filesystem-info?
          filesystem-info
          filesystem-info-path
          filesystem-info-device
          filesystem-info-type
          filesystem-info-block-size
          filesystem-info-blocks
          filesystem-info-blocks-free
          filesystem-info-blocks-available
          filesystem-info-files
          filesystem-info-files-free
          filesystem-info-read-only?
          filesystem-total-bytes filesystem-free-bytes filesystem-available-bytes

          ;; mounted filesystem records
          mounted-filesystem?
          mounted-filesystems
          mounted-filesystem-source
          mounted-filesystem-target
          mounted-filesystem-type
          mounted-filesystem-options)
  (import (cspp system private filesystem-info)
          (cspp system private filesystem-mounts)))
