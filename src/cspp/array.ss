(library (cspp array)
  (export array make-array array? array-size array-empty?
          array-ref array-add! array-add*! array-delete! array-set! array-clear!
          array-slice array-slice! array-copy array-copy!
          array-push! array-pop! array-push-back! array-pop-back!
          array-filter array-filter! array-partition
          array-contains? array-contains/p? array-index-of array-find-index
          array-search array-search*
          array-append array-append!
          array-reverse array-reverse!
          array-map array-map/i array-map! array-map/i!
          array-for-each array-for-each/i
          array-map-rev array-map/i-rev
          array-for-each-rev array-for-each/i-rev
          array-andmap array-ormap
          array-fold-left array-fold-left/i array-fold-right array-fold-right/i
          array-sorted? array-sort array-sort!
          array-iota array-nums


          fxarray make-fxarray fxarray? fxarray-size fxarray-empty?
          fxarray-ref fxarray-add! fxarray-add*! fxarray-delete! fxarray-set! fxarray-clear!
          fxarray-slice fxarray-slice! fxarray-copy fxarray-copy!
          fxarray-push! fxarray-pop! fxarray-push-back! fxarray-pop-back!
          fxarray-filter fxarray-filter! fxarray-partition
          fxarray-contains? fxarray-contains/p? fxarray-index-of fxarray-find-index
          fxarray-search fxarray-search*
          fxarray-append fxarray-append!
          fxarray-reverse fxarray-reverse!
          fxarray-map fxarray-map/i fxarray-map! fxarray-map/i!
          fxarray-for-each fxarray-for-each/i
          fxarray-map-rev fxarray-map/i-rev
          fxarray-for-each-rev fxarray-for-each/i-rev
          fxarray-andmap fxarray-ormap
          fxarray-fold-left fxarray-fold-left/i fxarray-fold-right fxarray-fold-right/i
          fxarray-sorted? fxarray-sort fxarray-sort!
          fxarray-iota fxarray-nums

          flarray make-flarray flarray? flarray-size flarray-empty?
          flarray-ref flarray-add! flarray-add*! flarray-delete! flarray-set! flarray-clear!
          flarray-slice flarray-slice! flarray-copy flarray-copy!
          flarray-push! flarray-pop! flarray-push-back! flarray-pop-back!
          flarray-filter flarray-filter! flarray-partition
          flarray-contains? flarray-contains/p? flarray-index-of flarray-find-index
          flarray-search flarray-search*
          flarray-append flarray-append!
          flarray-reverse flarray-reverse!
          flarray-map flarray-map/i flarray-map! flarray-map/i!
          flarray-for-each flarray-for-each/i
          flarray-map-rev flarray-map/i-rev
          flarray-for-each-rev flarray-for-each/i-rev
          flarray-andmap flarray-ormap
          flarray-fold-left flarray-fold-left/i flarray-fold-right flarray-fold-right/i
          flarray-sorted? flarray-sort flarray-sort!
          flarray-iota flarray-nums
          flarray->list flarray->iter flarray->flvector flvector->flarray

          bytearray make-bytearray bytearray? bytearray-size bytearray-empty?
          bytearray-ref bytearray-add! bytearray-add*! bytearray-delete! bytearray-set! bytearray-clear!
          bytearray-slice bytearray-slice! bytearray-copy bytearray-copy!
          bytearray-push! bytearray-pop! bytearray-push-back! bytearray-pop-back!
          bytearray-filter bytearray-filter! bytearray-partition
          bytearray-contains? bytearray-contains/p? bytearray-index-of bytearray-find-index
          bytearray-search bytearray-search*
          bytearray-append bytearray-append!
          bytearray-reverse bytearray-reverse!
          bytearray-map bytearray-map/i bytearray-map! bytearray-map/i!
          bytearray-for-each bytearray-for-each/i
          bytearray-map-rev bytearray-map/i-rev
          bytearray-for-each-rev bytearray-for-each/i-rev
          bytearray-andmap bytearray-ormap
          bytearray-fold-left bytearray-fold-left/i bytearray-fold-right bytearray-fold-right/i
          bytearray-sorted? bytearray-sort bytearray-sort!
          bytearray-iota bytearray-nums

          bytearray-u8-ref bytearray-U8-ref bytearray-u8-set! bytearray-U8-set!
          bytearray-s8-ref bytearray-S8-ref bytearray-s8-set! bytearray-S8-set!
          bytearray-u16-ref bytearray-U16-ref bytearray-u16-set! bytearray-U16-set!
          bytearray-s16-ref bytearray-S16-ref bytearray-s16-set! bytearray-S16-set!
          bytearray-u24-ref bytearray-U24-ref bytearray-u24-set! bytearray-U24-set!
          bytearray-s24-ref bytearray-S24-ref bytearray-s24-set! bytearray-S24-set!
          bytearray-u32-ref bytearray-U32-ref bytearray-u32-set! bytearray-U32-set!
          bytearray-s32-ref bytearray-S32-ref bytearray-s32-set! bytearray-S32-set!
          bytearray-u40-ref bytearray-U40-ref bytearray-u40-set! bytearray-U40-set!
          bytearray-s40-ref bytearray-S40-ref bytearray-s40-set! bytearray-S40-set!
          bytearray-u48-ref bytearray-U48-ref bytearray-u48-set! bytearray-U48-set!
          bytearray-s48-ref bytearray-S48-ref bytearray-s48-set! bytearray-S48-set!
          bytearray-u56-ref bytearray-U56-ref bytearray-u56-set! bytearray-U56-set!
          bytearray-s56-ref bytearray-S56-ref bytearray-s56-set! bytearray-S56-set!
          bytearray-u64-ref bytearray-U64-ref bytearray-u64-set! bytearray-U64-set!
          bytearray-s64-ref bytearray-S64-ref bytearray-s64-set! bytearray-S64-set!
          bytearray-fp32-ref bytearray-FP32-ref bytearray-fp32-set! bytearray-FP32-set!
          bytearray-fp64-ref bytearray-FP64-ref bytearray-fp64-set! bytearray-FP64-set!

          bytearray-u8-add! bytearray-u8-add*! bytearray-u8-delete! bytearray-u8-slice
          bytearray-u8-slice! bytearray-u8-copy bytearray-u8-copy! bytearray-u8-push!
          bytearray-u8-pop! bytearray-u8-push-back! bytearray-u8-pop-back! bytearray-u8-filter
          bytearray-u8-filter! bytearray-u8-partition bytearray-u8-contains? bytearray-u8-contains/p?
          bytearray-u8-index-of bytearray-u8-find-index bytearray-u8-search bytearray-u8-search*
          bytearray-u8-append bytearray-u8-append! bytearray-u8-reverse bytearray-u8-reverse!
          bytearray-u8-map bytearray-u8-map/i bytearray-u8-map! bytearray-u8-map/i!
          bytearray-u8-for-each bytearray-u8-for-each/i bytearray-u8-map-rev bytearray-u8-map/i-rev
          bytearray-u8-for-each-rev bytearray-u8-for-each/i-rev bytearray-u8-andmap bytearray-u8-ormap
          bytearray-u8-fold-left bytearray-u8-fold-left/i bytearray-u8-fold-right bytearray-u8-fold-right/i
          bytearray-u8-sorted? bytearray-u8-sort bytearray-u8-sort! bytearray-u8->list
          bytearray-u8->iter bytearray-u8->bytevector bytearray-u8-iota bytearray-u8-nums
          bytearray-U8-add! bytearray-U8-add*! bytearray-U8-delete! bytearray-U8-slice
          bytearray-U8-slice! bytearray-U8-copy bytearray-U8-copy! bytearray-U8-push!
          bytearray-U8-pop! bytearray-U8-push-back! bytearray-U8-pop-back! bytearray-U8-filter
          bytearray-U8-filter! bytearray-U8-partition bytearray-U8-contains? bytearray-U8-contains/p?
          bytearray-U8-index-of bytearray-U8-find-index bytearray-U8-search bytearray-U8-search*
          bytearray-U8-append bytearray-U8-append! bytearray-U8-reverse bytearray-U8-reverse!
          bytearray-U8-map bytearray-U8-map/i bytearray-U8-map! bytearray-U8-map/i!
          bytearray-U8-for-each bytearray-U8-for-each/i bytearray-U8-map-rev bytearray-U8-map/i-rev
          bytearray-U8-for-each-rev bytearray-U8-for-each/i-rev bytearray-U8-andmap bytearray-U8-ormap
          bytearray-U8-fold-left bytearray-U8-fold-left/i bytearray-U8-fold-right bytearray-U8-fold-right/i
          bytearray-U8-sorted? bytearray-U8-sort bytearray-U8-sort! bytearray-U8->list
          bytearray-U8->iter bytearray-U8->bytevector bytearray-U8-iota bytearray-U8-nums

          bytearray-s8-add! bytearray-s8-add*! bytearray-s8-delete! bytearray-s8-slice
          bytearray-s8-slice! bytearray-s8-copy bytearray-s8-copy! bytearray-s8-push!
          bytearray-s8-pop! bytearray-s8-push-back! bytearray-s8-pop-back! bytearray-s8-filter
          bytearray-s8-filter! bytearray-s8-partition bytearray-s8-contains? bytearray-s8-contains/p?
          bytearray-s8-index-of bytearray-s8-find-index bytearray-s8-search bytearray-s8-search*
          bytearray-s8-append bytearray-s8-append! bytearray-s8-reverse bytearray-s8-reverse!
          bytearray-s8-map bytearray-s8-map/i bytearray-s8-map! bytearray-s8-map/i!
          bytearray-s8-for-each bytearray-s8-for-each/i bytearray-s8-map-rev bytearray-s8-map/i-rev
          bytearray-s8-for-each-rev bytearray-s8-for-each/i-rev bytearray-s8-andmap bytearray-s8-ormap
          bytearray-s8-fold-left bytearray-s8-fold-left/i bytearray-s8-fold-right bytearray-s8-fold-right/i
          bytearray-s8-sorted? bytearray-s8-sort bytearray-s8-sort! bytearray-s8->list
          bytearray-s8->iter bytearray-s8->bytevector bytearray-s8-iota bytearray-s8-nums

          bytearray-S8-add! bytearray-S8-add*! bytearray-S8-delete! bytearray-S8-slice
          bytearray-S8-slice! bytearray-S8-copy bytearray-S8-copy! bytearray-S8-push!
          bytearray-S8-pop! bytearray-S8-push-back! bytearray-S8-pop-back! bytearray-S8-filter
          bytearray-S8-filter! bytearray-S8-partition bytearray-S8-contains? bytearray-S8-contains/p?
          bytearray-S8-index-of bytearray-S8-find-index bytearray-S8-search bytearray-S8-search*
          bytearray-S8-append bytearray-S8-append! bytearray-S8-reverse bytearray-S8-reverse!
          bytearray-S8-map bytearray-S8-map/i bytearray-S8-map! bytearray-S8-map/i!
          bytearray-S8-for-each bytearray-S8-for-each/i bytearray-S8-map-rev bytearray-S8-map/i-rev
          bytearray-S8-for-each-rev bytearray-S8-for-each/i-rev bytearray-S8-andmap bytearray-S8-ormap
          bytearray-S8-fold-left bytearray-S8-fold-left/i bytearray-S8-fold-right bytearray-S8-fold-right/i
          bytearray-S8-sorted? bytearray-S8-sort bytearray-S8-sort! bytearray-S8->list
          bytearray-S8->iter bytearray-S8->bytevector bytearray-S8-iota bytearray-S8-nums

          bytearray-u16-add! bytearray-u16-add*! bytearray-u16-delete! bytearray-u16-slice
          bytearray-u16-slice! bytearray-u16-copy bytearray-u16-copy! bytearray-u16-push!
          bytearray-u16-pop! bytearray-u16-push-back! bytearray-u16-pop-back! bytearray-u16-filter
          bytearray-u16-filter! bytearray-u16-partition bytearray-u16-contains? bytearray-u16-contains/p?
          bytearray-u16-index-of bytearray-u16-find-index bytearray-u16-search bytearray-u16-search*
          bytearray-u16-append bytearray-u16-append! bytearray-u16-reverse bytearray-u16-reverse!
          bytearray-u16-map bytearray-u16-map/i bytearray-u16-map! bytearray-u16-map/i!
          bytearray-u16-for-each bytearray-u16-for-each/i bytearray-u16-map-rev bytearray-u16-map/i-rev
          bytearray-u16-for-each-rev bytearray-u16-for-each/i-rev bytearray-u16-andmap bytearray-u16-ormap
          bytearray-u16-fold-left bytearray-u16-fold-left/i bytearray-u16-fold-right bytearray-u16-fold-right/i
          bytearray-u16-sorted? bytearray-u16-sort bytearray-u16-sort! bytearray-u16->list
          bytearray-u16->iter bytearray-u16->bytevector bytearray-u16-iota bytearray-u16-nums

          bytearray-U16-add! bytearray-U16-add*! bytearray-U16-delete! bytearray-U16-slice
          bytearray-U16-slice! bytearray-U16-copy bytearray-U16-copy! bytearray-U16-push!
          bytearray-U16-pop! bytearray-U16-push-back! bytearray-U16-pop-back! bytearray-U16-filter
          bytearray-U16-filter! bytearray-U16-partition bytearray-U16-contains? bytearray-U16-contains/p?
          bytearray-U16-index-of bytearray-U16-find-index bytearray-U16-search bytearray-U16-search*
          bytearray-U16-append bytearray-U16-append! bytearray-U16-reverse bytearray-U16-reverse!
          bytearray-U16-map bytearray-U16-map/i bytearray-U16-map! bytearray-U16-map/i!
          bytearray-U16-for-each bytearray-U16-for-each/i bytearray-U16-map-rev bytearray-U16-map/i-rev
          bytearray-U16-for-each-rev bytearray-U16-for-each/i-rev bytearray-U16-andmap bytearray-U16-ormap
          bytearray-U16-fold-left bytearray-U16-fold-left/i bytearray-U16-fold-right bytearray-U16-fold-right/i
          bytearray-U16-sorted? bytearray-U16-sort bytearray-U16-sort! bytearray-U16->list
          bytearray-U16->iter bytearray-U16->bytevector bytearray-U16-iota bytearray-U16-nums

          bytearray-s16-add! bytearray-s16-add*! bytearray-s16-delete! bytearray-s16-slice
          bytearray-s16-slice! bytearray-s16-copy bytearray-s16-copy! bytearray-s16-push!
          bytearray-s16-pop! bytearray-s16-push-back! bytearray-s16-pop-back! bytearray-s16-filter
          bytearray-s16-filter! bytearray-s16-partition bytearray-s16-contains? bytearray-s16-contains/p?
          bytearray-s16-index-of bytearray-s16-find-index bytearray-s16-search bytearray-s16-search*
          bytearray-s16-append bytearray-s16-append! bytearray-s16-reverse bytearray-s16-reverse!
          bytearray-s16-map bytearray-s16-map/i bytearray-s16-map! bytearray-s16-map/i!
          bytearray-s16-for-each bytearray-s16-for-each/i bytearray-s16-map-rev bytearray-s16-map/i-rev
          bytearray-s16-for-each-rev bytearray-s16-for-each/i-rev bytearray-s16-andmap bytearray-s16-ormap
          bytearray-s16-fold-left bytearray-s16-fold-left/i bytearray-s16-fold-right bytearray-s16-fold-right/i
          bytearray-s16-sorted? bytearray-s16-sort bytearray-s16-sort! bytearray-s16->list
          bytearray-s16->iter bytearray-s16->bytevector bytearray-s16-iota bytearray-s16-nums

          bytearray-S16-add! bytearray-S16-add*! bytearray-S16-delete! bytearray-S16-slice
          bytearray-S16-slice! bytearray-S16-copy bytearray-S16-copy! bytearray-S16-push!
          bytearray-S16-pop! bytearray-S16-push-back! bytearray-S16-pop-back! bytearray-S16-filter
          bytearray-S16-filter! bytearray-S16-partition bytearray-S16-contains? bytearray-S16-contains/p?
          bytearray-S16-index-of bytearray-S16-find-index bytearray-S16-search bytearray-S16-search*
          bytearray-S16-append bytearray-S16-append! bytearray-S16-reverse bytearray-S16-reverse!
          bytearray-S16-map bytearray-S16-map/i bytearray-S16-map! bytearray-S16-map/i!
          bytearray-S16-for-each bytearray-S16-for-each/i bytearray-S16-map-rev bytearray-S16-map/i-rev
          bytearray-S16-for-each-rev bytearray-S16-for-each/i-rev bytearray-S16-andmap bytearray-S16-ormap
          bytearray-S16-fold-left bytearray-S16-fold-left/i bytearray-S16-fold-right bytearray-S16-fold-right/i
          bytearray-S16-sorted? bytearray-S16-sort bytearray-S16-sort! bytearray-S16->list
          bytearray-S16->iter bytearray-S16->bytevector bytearray-S16-iota bytearray-S16-nums

          bytearray-u24-add! bytearray-u24-add*! bytearray-u24-delete! bytearray-u24-slice
          bytearray-u24-slice! bytearray-u24-copy bytearray-u24-copy! bytearray-u24-push!
          bytearray-u24-pop! bytearray-u24-push-back! bytearray-u24-pop-back! bytearray-u24-filter
          bytearray-u24-filter! bytearray-u24-partition bytearray-u24-contains? bytearray-u24-contains/p?
          bytearray-u24-index-of bytearray-u24-find-index bytearray-u24-search bytearray-u24-search*
          bytearray-u24-append bytearray-u24-append! bytearray-u24-reverse bytearray-u24-reverse!
          bytearray-u24-map bytearray-u24-map/i bytearray-u24-map! bytearray-u24-map/i!
          bytearray-u24-for-each bytearray-u24-for-each/i bytearray-u24-map-rev bytearray-u24-map/i-rev
          bytearray-u24-for-each-rev bytearray-u24-for-each/i-rev bytearray-u24-andmap bytearray-u24-ormap
          bytearray-u24-fold-left bytearray-u24-fold-left/i bytearray-u24-fold-right bytearray-u24-fold-right/i
          bytearray-u24-sorted? bytearray-u24-sort bytearray-u24-sort! bytearray-u24->list
          bytearray-u24->iter bytearray-u24->bytevector bytearray-u24-iota bytearray-u24-nums

          bytearray-U24-add! bytearray-U24-add*! bytearray-U24-delete! bytearray-U24-slice
          bytearray-U24-slice! bytearray-U24-copy bytearray-U24-copy! bytearray-U24-push!
          bytearray-U24-pop! bytearray-U24-push-back! bytearray-U24-pop-back! bytearray-U24-filter
          bytearray-U24-filter! bytearray-U24-partition bytearray-U24-contains? bytearray-U24-contains/p?
          bytearray-U24-index-of bytearray-U24-find-index bytearray-U24-search bytearray-U24-search*
          bytearray-U24-append bytearray-U24-append! bytearray-U24-reverse bytearray-U24-reverse!
          bytearray-U24-map bytearray-U24-map/i bytearray-U24-map! bytearray-U24-map/i!
          bytearray-U24-for-each bytearray-U24-for-each/i bytearray-U24-map-rev bytearray-U24-map/i-rev
          bytearray-U24-for-each-rev bytearray-U24-for-each/i-rev bytearray-U24-andmap bytearray-U24-ormap
          bytearray-U24-fold-left bytearray-U24-fold-left/i bytearray-U24-fold-right bytearray-U24-fold-right/i
          bytearray-U24-sorted? bytearray-U24-sort bytearray-U24-sort! bytearray-U24->list
          bytearray-U24->iter bytearray-U24->bytevector bytearray-U24-iota bytearray-U24-nums

          bytearray-s24-add! bytearray-s24-add*! bytearray-s24-delete! bytearray-s24-slice
          bytearray-s24-slice! bytearray-s24-copy bytearray-s24-copy! bytearray-s24-push!
          bytearray-s24-pop! bytearray-s24-push-back! bytearray-s24-pop-back! bytearray-s24-filter
          bytearray-s24-filter! bytearray-s24-partition bytearray-s24-contains? bytearray-s24-contains/p?
          bytearray-s24-index-of bytearray-s24-find-index bytearray-s24-search bytearray-s24-search*
          bytearray-s24-append bytearray-s24-append! bytearray-s24-reverse bytearray-s24-reverse!
          bytearray-s24-map bytearray-s24-map/i bytearray-s24-map! bytearray-s24-map/i!
          bytearray-s24-for-each bytearray-s24-for-each/i bytearray-s24-map-rev bytearray-s24-map/i-rev
          bytearray-s24-for-each-rev bytearray-s24-for-each/i-rev bytearray-s24-andmap bytearray-s24-ormap
          bytearray-s24-fold-left bytearray-s24-fold-left/i bytearray-s24-fold-right bytearray-s24-fold-right/i
          bytearray-s24-sorted? bytearray-s24-sort bytearray-s24-sort! bytearray-s24->list
          bytearray-s24->iter bytearray-s24->bytevector bytearray-s24-iota bytearray-s24-nums

          bytearray-S24-add! bytearray-S24-add*! bytearray-S24-delete! bytearray-S24-slice
          bytearray-S24-slice! bytearray-S24-copy bytearray-S24-copy! bytearray-S24-push!
          bytearray-S24-pop! bytearray-S24-push-back! bytearray-S24-pop-back! bytearray-S24-filter
          bytearray-S24-filter! bytearray-S24-partition bytearray-S24-contains? bytearray-S24-contains/p?
          bytearray-S24-index-of bytearray-S24-find-index bytearray-S24-search bytearray-S24-search*
          bytearray-S24-append bytearray-S24-append! bytearray-S24-reverse bytearray-S24-reverse!
          bytearray-S24-map bytearray-S24-map/i bytearray-S24-map! bytearray-S24-map/i!
          bytearray-S24-for-each bytearray-S24-for-each/i bytearray-S24-map-rev bytearray-S24-map/i-rev
          bytearray-S24-for-each-rev bytearray-S24-for-each/i-rev bytearray-S24-andmap bytearray-S24-ormap
          bytearray-S24-fold-left bytearray-S24-fold-left/i bytearray-S24-fold-right bytearray-S24-fold-right/i
          bytearray-S24-sorted? bytearray-S24-sort bytearray-S24-sort! bytearray-S24->list
          bytearray-S24->iter bytearray-S24->bytevector bytearray-S24-iota bytearray-S24-nums

          bytearray-u32-add! bytearray-u32-add*! bytearray-u32-delete! bytearray-u32-slice
          bytearray-u32-slice! bytearray-u32-copy bytearray-u32-copy! bytearray-u32-push!
          bytearray-u32-pop! bytearray-u32-push-back! bytearray-u32-pop-back! bytearray-u32-filter
          bytearray-u32-filter! bytearray-u32-partition bytearray-u32-contains? bytearray-u32-contains/p?
          bytearray-u32-index-of bytearray-u32-find-index bytearray-u32-search bytearray-u32-search*
          bytearray-u32-append bytearray-u32-append! bytearray-u32-reverse bytearray-u32-reverse!
          bytearray-u32-map bytearray-u32-map/i bytearray-u32-map! bytearray-u32-map/i!
          bytearray-u32-for-each bytearray-u32-for-each/i bytearray-u32-map-rev bytearray-u32-map/i-rev
          bytearray-u32-for-each-rev bytearray-u32-for-each/i-rev bytearray-u32-andmap bytearray-u32-ormap
          bytearray-u32-fold-left bytearray-u32-fold-left/i bytearray-u32-fold-right bytearray-u32-fold-right/i
          bytearray-u32-sorted? bytearray-u32-sort bytearray-u32-sort! bytearray-u32->list
          bytearray-u32->iter bytearray-u32->bytevector bytearray-u32-iota bytearray-u32-nums

          bytearray-U32-add! bytearray-U32-add*! bytearray-U32-delete! bytearray-U32-slice
          bytearray-U32-slice! bytearray-U32-copy bytearray-U32-copy! bytearray-U32-push!
          bytearray-U32-pop! bytearray-U32-push-back! bytearray-U32-pop-back! bytearray-U32-filter
          bytearray-U32-filter! bytearray-U32-partition bytearray-U32-contains? bytearray-U32-contains/p?
          bytearray-U32-index-of bytearray-U32-find-index bytearray-U32-search bytearray-U32-search*
          bytearray-U32-append bytearray-U32-append! bytearray-U32-reverse bytearray-U32-reverse!
          bytearray-U32-map bytearray-U32-map/i bytearray-U32-map! bytearray-U32-map/i!
          bytearray-U32-for-each bytearray-U32-for-each/i bytearray-U32-map-rev bytearray-U32-map/i-rev
          bytearray-U32-for-each-rev bytearray-U32-for-each/i-rev bytearray-U32-andmap bytearray-U32-ormap
          bytearray-U32-fold-left bytearray-U32-fold-left/i bytearray-U32-fold-right bytearray-U32-fold-right/i
          bytearray-U32-sorted? bytearray-U32-sort bytearray-U32-sort! bytearray-U32->list
          bytearray-U32->iter bytearray-U32->bytevector bytearray-U32-iota bytearray-U32-nums

          bytearray-s32-add! bytearray-s32-add*! bytearray-s32-delete! bytearray-s32-slice
          bytearray-s32-slice! bytearray-s32-copy bytearray-s32-copy! bytearray-s32-push!
          bytearray-s32-pop! bytearray-s32-push-back! bytearray-s32-pop-back! bytearray-s32-filter
          bytearray-s32-filter! bytearray-s32-partition bytearray-s32-contains? bytearray-s32-contains/p?
          bytearray-s32-index-of bytearray-s32-find-index bytearray-s32-search bytearray-s32-search*
          bytearray-s32-append bytearray-s32-append! bytearray-s32-reverse bytearray-s32-reverse!
          bytearray-s32-map bytearray-s32-map/i bytearray-s32-map! bytearray-s32-map/i!
          bytearray-s32-for-each bytearray-s32-for-each/i bytearray-s32-map-rev bytearray-s32-map/i-rev
          bytearray-s32-for-each-rev bytearray-s32-for-each/i-rev bytearray-s32-andmap bytearray-s32-ormap
          bytearray-s32-fold-left bytearray-s32-fold-left/i bytearray-s32-fold-right bytearray-s32-fold-right/i
          bytearray-s32-sorted? bytearray-s32-sort bytearray-s32-sort! bytearray-s32->list
          bytearray-s32->iter bytearray-s32->bytevector bytearray-s32-iota bytearray-s32-nums

          bytearray-S32-add! bytearray-S32-add*! bytearray-S32-delete! bytearray-S32-slice
          bytearray-S32-slice! bytearray-S32-copy bytearray-S32-copy! bytearray-S32-push!
          bytearray-S32-pop! bytearray-S32-push-back! bytearray-S32-pop-back! bytearray-S32-filter
          bytearray-S32-filter! bytearray-S32-partition bytearray-S32-contains? bytearray-S32-contains/p?
          bytearray-S32-index-of bytearray-S32-find-index bytearray-S32-search bytearray-S32-search*
          bytearray-S32-append bytearray-S32-append! bytearray-S32-reverse bytearray-S32-reverse!
          bytearray-S32-map bytearray-S32-map/i bytearray-S32-map! bytearray-S32-map/i!
          bytearray-S32-for-each bytearray-S32-for-each/i bytearray-S32-map-rev bytearray-S32-map/i-rev
          bytearray-S32-for-each-rev bytearray-S32-for-each/i-rev bytearray-S32-andmap bytearray-S32-ormap
          bytearray-S32-fold-left bytearray-S32-fold-left/i bytearray-S32-fold-right bytearray-S32-fold-right/i
          bytearray-S32-sorted? bytearray-S32-sort bytearray-S32-sort! bytearray-S32->list
          bytearray-S32->iter bytearray-S32->bytevector bytearray-S32-iota bytearray-S32-nums

          bytearray-u40-add! bytearray-u40-add*! bytearray-u40-delete! bytearray-u40-slice
          bytearray-u40-slice! bytearray-u40-copy bytearray-u40-copy! bytearray-u40-push!
          bytearray-u40-pop! bytearray-u40-push-back! bytearray-u40-pop-back! bytearray-u40-filter
          bytearray-u40-filter! bytearray-u40-partition bytearray-u40-contains? bytearray-u40-contains/p?
          bytearray-u40-index-of bytearray-u40-find-index bytearray-u40-search bytearray-u40-search*
          bytearray-u40-append bytearray-u40-append! bytearray-u40-reverse bytearray-u40-reverse!
          bytearray-u40-map bytearray-u40-map/i bytearray-u40-map! bytearray-u40-map/i!
          bytearray-u40-for-each bytearray-u40-for-each/i bytearray-u40-map-rev bytearray-u40-map/i-rev
          bytearray-u40-for-each-rev bytearray-u40-for-each/i-rev bytearray-u40-andmap bytearray-u40-ormap
          bytearray-u40-fold-left bytearray-u40-fold-left/i bytearray-u40-fold-right bytearray-u40-fold-right/i
          bytearray-u40-sorted? bytearray-u40-sort bytearray-u40-sort! bytearray-u40->list
          bytearray-u40->iter bytearray-u40->bytevector bytearray-u40-iota bytearray-u40-nums

          bytearray-U40-add! bytearray-U40-add*! bytearray-U40-delete! bytearray-U40-slice
          bytearray-U40-slice! bytearray-U40-copy bytearray-U40-copy! bytearray-U40-push!
          bytearray-U40-pop! bytearray-U40-push-back! bytearray-U40-pop-back! bytearray-U40-filter
          bytearray-U40-filter! bytearray-U40-partition bytearray-U40-contains? bytearray-U40-contains/p?
          bytearray-U40-index-of bytearray-U40-find-index bytearray-U40-search bytearray-U40-search*
          bytearray-U40-append bytearray-U40-append! bytearray-U40-reverse bytearray-U40-reverse!
          bytearray-U40-map bytearray-U40-map/i bytearray-U40-map! bytearray-U40-map/i!
          bytearray-U40-for-each bytearray-U40-for-each/i bytearray-U40-map-rev bytearray-U40-map/i-rev
          bytearray-U40-for-each-rev bytearray-U40-for-each/i-rev bytearray-U40-andmap bytearray-U40-ormap
          bytearray-U40-fold-left bytearray-U40-fold-left/i bytearray-U40-fold-right bytearray-U40-fold-right/i
          bytearray-U40-sorted? bytearray-U40-sort bytearray-U40-sort! bytearray-U40->list
          bytearray-U40->iter bytearray-U40->bytevector bytearray-U40-iota bytearray-U40-nums

          bytearray-s40-add! bytearray-s40-add*! bytearray-s40-delete! bytearray-s40-slice
          bytearray-s40-slice! bytearray-s40-copy bytearray-s40-copy! bytearray-s40-push!
          bytearray-s40-pop! bytearray-s40-push-back! bytearray-s40-pop-back! bytearray-s40-filter
          bytearray-s40-filter! bytearray-s40-partition bytearray-s40-contains? bytearray-s40-contains/p?
          bytearray-s40-index-of bytearray-s40-find-index bytearray-s40-search bytearray-s40-search*
          bytearray-s40-append bytearray-s40-append! bytearray-s40-reverse bytearray-s40-reverse!
          bytearray-s40-map bytearray-s40-map/i bytearray-s40-map! bytearray-s40-map/i!
          bytearray-s40-for-each bytearray-s40-for-each/i bytearray-s40-map-rev bytearray-s40-map/i-rev
          bytearray-s40-for-each-rev bytearray-s40-for-each/i-rev bytearray-s40-andmap bytearray-s40-ormap
          bytearray-s40-fold-left bytearray-s40-fold-left/i bytearray-s40-fold-right bytearray-s40-fold-right/i
          bytearray-s40-sorted? bytearray-s40-sort bytearray-s40-sort! bytearray-s40->list
          bytearray-s40->iter bytearray-s40->bytevector bytearray-s40-iota bytearray-s40-nums

          bytearray-S40-add! bytearray-S40-add*! bytearray-S40-delete! bytearray-S40-slice
          bytearray-S40-slice! bytearray-S40-copy bytearray-S40-copy! bytearray-S40-push!
          bytearray-S40-pop! bytearray-S40-push-back! bytearray-S40-pop-back! bytearray-S40-filter
          bytearray-S40-filter! bytearray-S40-partition bytearray-S40-contains? bytearray-S40-contains/p?
          bytearray-S40-index-of bytearray-S40-find-index bytearray-S40-search bytearray-S40-search*
          bytearray-S40-append bytearray-S40-append! bytearray-S40-reverse bytearray-S40-reverse!
          bytearray-S40-map bytearray-S40-map/i bytearray-S40-map! bytearray-S40-map/i!
          bytearray-S40-for-each bytearray-S40-for-each/i bytearray-S40-map-rev bytearray-S40-map/i-rev
          bytearray-S40-for-each-rev bytearray-S40-for-each/i-rev bytearray-S40-andmap bytearray-S40-ormap
          bytearray-S40-fold-left bytearray-S40-fold-left/i bytearray-S40-fold-right bytearray-S40-fold-right/i
          bytearray-S40-sorted? bytearray-S40-sort bytearray-S40-sort! bytearray-S40->list
          bytearray-S40->iter bytearray-S40->bytevector bytearray-S40-iota bytearray-S40-nums

          bytearray-u48-add! bytearray-u48-add*! bytearray-u48-delete! bytearray-u48-slice
          bytearray-u48-slice! bytearray-u48-copy bytearray-u48-copy! bytearray-u48-push!
          bytearray-u48-pop! bytearray-u48-push-back! bytearray-u48-pop-back! bytearray-u48-filter
          bytearray-u48-filter! bytearray-u48-partition bytearray-u48-contains? bytearray-u48-contains/p?
          bytearray-u48-index-of bytearray-u48-find-index bytearray-u48-search bytearray-u48-search*
          bytearray-u48-append bytearray-u48-append! bytearray-u48-reverse bytearray-u48-reverse!
          bytearray-u48-map bytearray-u48-map/i bytearray-u48-map! bytearray-u48-map/i!
          bytearray-u48-for-each bytearray-u48-for-each/i bytearray-u48-map-rev bytearray-u48-map/i-rev
          bytearray-u48-for-each-rev bytearray-u48-for-each/i-rev bytearray-u48-andmap bytearray-u48-ormap
          bytearray-u48-fold-left bytearray-u48-fold-left/i bytearray-u48-fold-right bytearray-u48-fold-right/i
          bytearray-u48-sorted? bytearray-u48-sort bytearray-u48-sort! bytearray-u48->list
          bytearray-u48->iter bytearray-u48->bytevector bytearray-u48-iota bytearray-u48-nums

          bytearray-U48-add! bytearray-U48-add*! bytearray-U48-delete! bytearray-U48-slice
          bytearray-U48-slice! bytearray-U48-copy bytearray-U48-copy! bytearray-U48-push!
          bytearray-U48-pop! bytearray-U48-push-back! bytearray-U48-pop-back! bytearray-U48-filter
          bytearray-U48-filter! bytearray-U48-partition bytearray-U48-contains? bytearray-U48-contains/p?
          bytearray-U48-index-of bytearray-U48-find-index bytearray-U48-search bytearray-U48-search*
          bytearray-U48-append bytearray-U48-append! bytearray-U48-reverse bytearray-U48-reverse!
          bytearray-U48-map bytearray-U48-map/i bytearray-U48-map! bytearray-U48-map/i!
          bytearray-U48-for-each bytearray-U48-for-each/i bytearray-U48-map-rev bytearray-U48-map/i-rev
          bytearray-U48-for-each-rev bytearray-U48-for-each/i-rev bytearray-U48-andmap bytearray-U48-ormap
          bytearray-U48-fold-left bytearray-U48-fold-left/i bytearray-U48-fold-right bytearray-U48-fold-right/i
          bytearray-U48-sorted? bytearray-U48-sort bytearray-U48-sort! bytearray-U48->list
          bytearray-U48->iter bytearray-U48->bytevector bytearray-U48-iota bytearray-U48-nums

          bytearray-s48-add! bytearray-s48-add*! bytearray-s48-delete! bytearray-s48-slice
          bytearray-s48-slice! bytearray-s48-copy bytearray-s48-copy! bytearray-s48-push!
          bytearray-s48-pop! bytearray-s48-push-back! bytearray-s48-pop-back! bytearray-s48-filter
          bytearray-s48-filter! bytearray-s48-partition bytearray-s48-contains? bytearray-s48-contains/p?
          bytearray-s48-index-of bytearray-s48-find-index bytearray-s48-search bytearray-s48-search*
          bytearray-s48-append bytearray-s48-append! bytearray-s48-reverse bytearray-s48-reverse!
          bytearray-s48-map bytearray-s48-map/i bytearray-s48-map! bytearray-s48-map/i!
          bytearray-s48-for-each bytearray-s48-for-each/i bytearray-s48-map-rev bytearray-s48-map/i-rev
          bytearray-s48-for-each-rev bytearray-s48-for-each/i-rev bytearray-s48-andmap bytearray-s48-ormap
          bytearray-s48-fold-left bytearray-s48-fold-left/i bytearray-s48-fold-right bytearray-s48-fold-right/i
          bytearray-s48-sorted? bytearray-s48-sort bytearray-s48-sort! bytearray-s48->list
          bytearray-s48->iter bytearray-s48->bytevector bytearray-s48-iota bytearray-s48-nums

          bytearray-S48-add! bytearray-S48-add*! bytearray-S48-delete! bytearray-S48-slice
          bytearray-S48-slice! bytearray-S48-copy bytearray-S48-copy! bytearray-S48-push!
          bytearray-S48-pop! bytearray-S48-push-back! bytearray-S48-pop-back! bytearray-S48-filter
          bytearray-S48-filter! bytearray-S48-partition bytearray-S48-contains? bytearray-S48-contains/p?
          bytearray-S48-index-of bytearray-S48-find-index bytearray-S48-search bytearray-S48-search*
          bytearray-S48-append bytearray-S48-append! bytearray-S48-reverse bytearray-S48-reverse!
          bytearray-S48-map bytearray-S48-map/i bytearray-S48-map! bytearray-S48-map/i!
          bytearray-S48-for-each bytearray-S48-for-each/i bytearray-S48-map-rev bytearray-S48-map/i-rev
          bytearray-S48-for-each-rev bytearray-S48-for-each/i-rev bytearray-S48-andmap bytearray-S48-ormap
          bytearray-S48-fold-left bytearray-S48-fold-left/i bytearray-S48-fold-right bytearray-S48-fold-right/i
          bytearray-S48-sorted? bytearray-S48-sort bytearray-S48-sort! bytearray-S48->list
          bytearray-S48->iter bytearray-S48->bytevector bytearray-S48-iota bytearray-S48-nums

          bytearray-u56-add! bytearray-u56-add*! bytearray-u56-delete! bytearray-u56-slice
          bytearray-u56-slice! bytearray-u56-copy bytearray-u56-copy! bytearray-u56-push!
          bytearray-u56-pop! bytearray-u56-push-back! bytearray-u56-pop-back! bytearray-u56-filter
          bytearray-u56-filter! bytearray-u56-partition bytearray-u56-contains? bytearray-u56-contains/p?
          bytearray-u56-index-of bytearray-u56-find-index bytearray-u56-search bytearray-u56-search*
          bytearray-u56-append bytearray-u56-append! bytearray-u56-reverse bytearray-u56-reverse!
          bytearray-u56-map bytearray-u56-map/i bytearray-u56-map! bytearray-u56-map/i!
          bytearray-u56-for-each bytearray-u56-for-each/i bytearray-u56-map-rev bytearray-u56-map/i-rev
          bytearray-u56-for-each-rev bytearray-u56-for-each/i-rev bytearray-u56-andmap bytearray-u56-ormap
          bytearray-u56-fold-left bytearray-u56-fold-left/i bytearray-u56-fold-right bytearray-u56-fold-right/i
          bytearray-u56-sorted? bytearray-u56-sort bytearray-u56-sort! bytearray-u56->list
          bytearray-u56->iter bytearray-u56->bytevector bytearray-u56-iota bytearray-u56-nums

          bytearray-U56-add! bytearray-U56-add*! bytearray-U56-delete! bytearray-U56-slice
          bytearray-U56-slice! bytearray-U56-copy bytearray-U56-copy! bytearray-U56-push!
          bytearray-U56-pop! bytearray-U56-push-back! bytearray-U56-pop-back! bytearray-U56-filter
          bytearray-U56-filter! bytearray-U56-partition bytearray-U56-contains? bytearray-U56-contains/p?
          bytearray-U56-index-of bytearray-U56-find-index bytearray-U56-search bytearray-U56-search*
          bytearray-U56-append bytearray-U56-append! bytearray-U56-reverse bytearray-U56-reverse!
          bytearray-U56-map bytearray-U56-map/i bytearray-U56-map! bytearray-U56-map/i!
          bytearray-U56-for-each bytearray-U56-for-each/i bytearray-U56-map-rev bytearray-U56-map/i-rev
          bytearray-U56-for-each-rev bytearray-U56-for-each/i-rev bytearray-U56-andmap bytearray-U56-ormap
          bytearray-U56-fold-left bytearray-U56-fold-left/i bytearray-U56-fold-right bytearray-U56-fold-right/i
          bytearray-U56-sorted? bytearray-U56-sort bytearray-U56-sort! bytearray-U56->list
          bytearray-U56->iter bytearray-U56->bytevector bytearray-U56-iota bytearray-U56-nums

          bytearray-s56-add! bytearray-s56-add*! bytearray-s56-delete! bytearray-s56-slice
          bytearray-s56-slice! bytearray-s56-copy bytearray-s56-copy! bytearray-s56-push!
          bytearray-s56-pop! bytearray-s56-push-back! bytearray-s56-pop-back! bytearray-s56-filter
          bytearray-s56-filter! bytearray-s56-partition bytearray-s56-contains? bytearray-s56-contains/p?
          bytearray-s56-index-of bytearray-s56-find-index bytearray-s56-search bytearray-s56-search*
          bytearray-s56-append bytearray-s56-append! bytearray-s56-reverse bytearray-s56-reverse!
          bytearray-s56-map bytearray-s56-map/i bytearray-s56-map! bytearray-s56-map/i!
          bytearray-s56-for-each bytearray-s56-for-each/i bytearray-s56-map-rev bytearray-s56-map/i-rev
          bytearray-s56-for-each-rev bytearray-s56-for-each/i-rev bytearray-s56-andmap bytearray-s56-ormap
          bytearray-s56-fold-left bytearray-s56-fold-left/i bytearray-s56-fold-right bytearray-s56-fold-right/i
          bytearray-s56-sorted? bytearray-s56-sort bytearray-s56-sort! bytearray-s56->list
          bytearray-s56->iter bytearray-s56->bytevector bytearray-s56-iota bytearray-s56-nums

          bytearray-S56-add! bytearray-S56-add*! bytearray-S56-delete! bytearray-S56-slice
          bytearray-S56-slice! bytearray-S56-copy bytearray-S56-copy! bytearray-S56-push!
          bytearray-S56-pop! bytearray-S56-push-back! bytearray-S56-pop-back! bytearray-S56-filter
          bytearray-S56-filter! bytearray-S56-partition bytearray-S56-contains? bytearray-S56-contains/p?
          bytearray-S56-index-of bytearray-S56-find-index bytearray-S56-search bytearray-S56-search*
          bytearray-S56-append bytearray-S56-append! bytearray-S56-reverse bytearray-S56-reverse!
          bytearray-S56-map bytearray-S56-map/i bytearray-S56-map! bytearray-S56-map/i!
          bytearray-S56-for-each bytearray-S56-for-each/i bytearray-S56-map-rev bytearray-S56-map/i-rev
          bytearray-S56-for-each-rev bytearray-S56-for-each/i-rev bytearray-S56-andmap bytearray-S56-ormap
          bytearray-S56-fold-left bytearray-S56-fold-left/i bytearray-S56-fold-right bytearray-S56-fold-right/i
          bytearray-S56-sorted? bytearray-S56-sort bytearray-S56-sort! bytearray-S56->list
          bytearray-S56->iter bytearray-S56->bytevector bytearray-S56-iota bytearray-S56-nums

          bytearray-u64-add! bytearray-u64-add*! bytearray-u64-delete! bytearray-u64-slice
          bytearray-u64-slice! bytearray-u64-copy bytearray-u64-copy! bytearray-u64-push!
          bytearray-u64-pop! bytearray-u64-push-back! bytearray-u64-pop-back! bytearray-u64-filter
          bytearray-u64-filter! bytearray-u64-partition bytearray-u64-contains? bytearray-u64-contains/p?
          bytearray-u64-index-of bytearray-u64-find-index bytearray-u64-search bytearray-u64-search*
          bytearray-u64-append bytearray-u64-append! bytearray-u64-reverse bytearray-u64-reverse!
          bytearray-u64-map bytearray-u64-map/i bytearray-u64-map! bytearray-u64-map/i!
          bytearray-u64-for-each bytearray-u64-for-each/i bytearray-u64-map-rev bytearray-u64-map/i-rev
          bytearray-u64-for-each-rev bytearray-u64-for-each/i-rev bytearray-u64-andmap bytearray-u64-ormap
          bytearray-u64-fold-left bytearray-u64-fold-left/i bytearray-u64-fold-right bytearray-u64-fold-right/i
          bytearray-u64-sorted? bytearray-u64-sort bytearray-u64-sort! bytearray-u64->list
          bytearray-u64->iter bytearray-u64->bytevector bytearray-u64-iota bytearray-u64-nums

          bytearray-U64-add! bytearray-U64-add*! bytearray-U64-delete! bytearray-U64-slice
          bytearray-U64-slice! bytearray-U64-copy bytearray-U64-copy! bytearray-U64-push!
          bytearray-U64-pop! bytearray-U64-push-back! bytearray-U64-pop-back! bytearray-U64-filter
          bytearray-U64-filter! bytearray-U64-partition bytearray-U64-contains? bytearray-U64-contains/p?
          bytearray-U64-index-of bytearray-U64-find-index bytearray-U64-search bytearray-U64-search*
          bytearray-U64-append bytearray-U64-append! bytearray-U64-reverse bytearray-U64-reverse!
          bytearray-U64-map bytearray-U64-map/i bytearray-U64-map! bytearray-U64-map/i!
          bytearray-U64-for-each bytearray-U64-for-each/i bytearray-U64-map-rev bytearray-U64-map/i-rev
          bytearray-U64-for-each-rev bytearray-U64-for-each/i-rev bytearray-U64-andmap bytearray-U64-ormap
          bytearray-U64-fold-left bytearray-U64-fold-left/i bytearray-U64-fold-right bytearray-U64-fold-right/i
          bytearray-U64-sorted? bytearray-U64-sort bytearray-U64-sort! bytearray-U64->list
          bytearray-U64->iter bytearray-U64->bytevector bytearray-U64-iota bytearray-U64-nums

          bytearray-s64-add! bytearray-s64-add*! bytearray-s64-delete! bytearray-s64-slice
          bytearray-s64-slice! bytearray-s64-copy bytearray-s64-copy! bytearray-s64-push!
          bytearray-s64-pop! bytearray-s64-push-back! bytearray-s64-pop-back! bytearray-s64-filter
          bytearray-s64-filter! bytearray-s64-partition bytearray-s64-contains? bytearray-s64-contains/p?
          bytearray-s64-index-of bytearray-s64-find-index bytearray-s64-search bytearray-s64-search*
          bytearray-s64-append bytearray-s64-append! bytearray-s64-reverse bytearray-s64-reverse!
          bytearray-s64-map bytearray-s64-map/i bytearray-s64-map! bytearray-s64-map/i!
          bytearray-s64-for-each bytearray-s64-for-each/i bytearray-s64-map-rev bytearray-s64-map/i-rev
          bytearray-s64-for-each-rev bytearray-s64-for-each/i-rev bytearray-s64-andmap bytearray-s64-ormap
          bytearray-s64-fold-left bytearray-s64-fold-left/i bytearray-s64-fold-right bytearray-s64-fold-right/i
          bytearray-s64-sorted? bytearray-s64-sort bytearray-s64-sort! bytearray-s64->list
          bytearray-s64->iter bytearray-s64->bytevector bytearray-s64-iota bytearray-s64-nums

          bytearray-S64-add! bytearray-S64-add*! bytearray-S64-delete! bytearray-S64-slice
          bytearray-S64-slice! bytearray-S64-copy bytearray-S64-copy! bytearray-S64-push!
          bytearray-S64-pop! bytearray-S64-push-back! bytearray-S64-pop-back! bytearray-S64-filter
          bytearray-S64-filter! bytearray-S64-partition bytearray-S64-contains? bytearray-S64-contains/p?
          bytearray-S64-index-of bytearray-S64-find-index bytearray-S64-search bytearray-S64-search*
          bytearray-S64-append bytearray-S64-append! bytearray-S64-reverse bytearray-S64-reverse!
          bytearray-S64-map bytearray-S64-map/i bytearray-S64-map! bytearray-S64-map/i!
          bytearray-S64-for-each bytearray-S64-for-each/i bytearray-S64-map-rev bytearray-S64-map/i-rev
          bytearray-S64-for-each-rev bytearray-S64-for-each/i-rev bytearray-S64-andmap bytearray-S64-ormap
          bytearray-S64-fold-left bytearray-S64-fold-left/i bytearray-S64-fold-right bytearray-S64-fold-right/i
          bytearray-S64-sorted? bytearray-S64-sort bytearray-S64-sort! bytearray-S64->list
          bytearray-S64->iter bytearray-S64->bytevector bytearray-S64-iota bytearray-S64-nums

          bytearray-fp32-add! bytearray-fp32-add*! bytearray-fp32-delete! bytearray-fp32-slice
          bytearray-fp32-slice! bytearray-fp32-copy bytearray-fp32-copy! bytearray-fp32-push!
          bytearray-fp32-pop! bytearray-fp32-push-back! bytearray-fp32-pop-back! bytearray-fp32-filter
          bytearray-fp32-filter! bytearray-fp32-partition bytearray-fp32-contains? bytearray-fp32-contains/p?
          bytearray-fp32-index-of bytearray-fp32-find-index bytearray-fp32-search bytearray-fp32-search*
          bytearray-fp32-append bytearray-fp32-append! bytearray-fp32-reverse bytearray-fp32-reverse!
          bytearray-fp32-map bytearray-fp32-map/i bytearray-fp32-map! bytearray-fp32-map/i!
          bytearray-fp32-for-each bytearray-fp32-for-each/i bytearray-fp32-map-rev bytearray-fp32-map/i-rev
          bytearray-fp32-for-each-rev bytearray-fp32-for-each/i-rev bytearray-fp32-andmap bytearray-fp32-ormap
          bytearray-fp32-fold-left bytearray-fp32-fold-left/i bytearray-fp32-fold-right bytearray-fp32-fold-right/i
          bytearray-fp32-sorted? bytearray-fp32-sort bytearray-fp32-sort! bytearray-fp32->list
          bytearray-fp32->iter bytearray-fp32->bytevector bytearray-fp32-iota bytearray-fp32-nums

          bytearray-FP32-add! bytearray-FP32-add*! bytearray-FP32-delete! bytearray-FP32-slice
          bytearray-FP32-slice! bytearray-FP32-copy bytearray-FP32-copy! bytearray-FP32-push!
          bytearray-FP32-pop! bytearray-FP32-push-back! bytearray-FP32-pop-back! bytearray-FP32-filter
          bytearray-FP32-filter! bytearray-FP32-partition bytearray-FP32-contains? bytearray-FP32-contains/p?
          bytearray-FP32-index-of bytearray-FP32-find-index bytearray-FP32-search bytearray-FP32-search*
          bytearray-FP32-append bytearray-FP32-append! bytearray-FP32-reverse bytearray-FP32-reverse!
          bytearray-FP32-map bytearray-FP32-map/i bytearray-FP32-map! bytearray-FP32-map/i!
          bytearray-FP32-for-each bytearray-FP32-for-each/i bytearray-FP32-map-rev bytearray-FP32-map/i-rev
          bytearray-FP32-for-each-rev bytearray-FP32-for-each/i-rev bytearray-FP32-andmap bytearray-FP32-ormap
          bytearray-FP32-fold-left bytearray-FP32-fold-left/i bytearray-FP32-fold-right bytearray-FP32-fold-right/i
          bytearray-FP32-sorted? bytearray-FP32-sort bytearray-FP32-sort! bytearray-FP32->list
          bytearray-FP32->iter bytearray-FP32->bytevector bytearray-FP32-iota bytearray-FP32-nums

          bytearray-fp64-add! bytearray-fp64-add*! bytearray-fp64-delete! bytearray-fp64-slice
          bytearray-fp64-slice! bytearray-fp64-copy bytearray-fp64-copy! bytearray-fp64-push!
          bytearray-fp64-pop! bytearray-fp64-push-back! bytearray-fp64-pop-back! bytearray-fp64-filter
          bytearray-fp64-filter! bytearray-fp64-partition bytearray-fp64-contains? bytearray-fp64-contains/p?
          bytearray-fp64-index-of bytearray-fp64-find-index bytearray-fp64-search bytearray-fp64-search*
          bytearray-fp64-append bytearray-fp64-append! bytearray-fp64-reverse bytearray-fp64-reverse!
          bytearray-fp64-map bytearray-fp64-map/i bytearray-fp64-map! bytearray-fp64-map/i!
          bytearray-fp64-for-each bytearray-fp64-for-each/i bytearray-fp64-map-rev bytearray-fp64-map/i-rev
          bytearray-fp64-for-each-rev bytearray-fp64-for-each/i-rev bytearray-fp64-andmap bytearray-fp64-ormap
          bytearray-fp64-fold-left bytearray-fp64-fold-left/i bytearray-fp64-fold-right bytearray-fp64-fold-right/i
          bytearray-fp64-sorted? bytearray-fp64-sort bytearray-fp64-sort! bytearray-fp64->list
          bytearray-fp64->iter bytearray-fp64->bytevector bytearray-fp64-iota bytearray-fp64-nums

          bytearray-FP64-add! bytearray-FP64-add*! bytearray-FP64-delete! bytearray-FP64-slice
          bytearray-FP64-slice! bytearray-FP64-copy bytearray-FP64-copy! bytearray-FP64-push!
          bytearray-FP64-pop! bytearray-FP64-push-back! bytearray-FP64-pop-back! bytearray-FP64-filter
          bytearray-FP64-filter! bytearray-FP64-partition bytearray-FP64-contains? bytearray-FP64-contains/p?
          bytearray-FP64-index-of bytearray-FP64-find-index bytearray-FP64-search bytearray-FP64-search*
          bytearray-FP64-append bytearray-FP64-append! bytearray-FP64-reverse bytearray-FP64-reverse!
          bytearray-FP64-map bytearray-FP64-map/i bytearray-FP64-map! bytearray-FP64-map/i!
          bytearray-FP64-for-each bytearray-FP64-for-each/i bytearray-FP64-map-rev bytearray-FP64-map/i-rev
          bytearray-FP64-for-each-rev bytearray-FP64-for-each/i-rev bytearray-FP64-andmap bytearray-FP64-ormap
          bytearray-FP64-fold-left bytearray-FP64-fold-left/i bytearray-FP64-fold-right bytearray-FP64-fold-right/i
          bytearray-FP64-sorted? bytearray-FP64-sort bytearray-FP64-sort! bytearray-FP64->list
          bytearray-FP64->iter bytearray-FP64->bytevector bytearray-FP64-iota bytearray-FP64-nums

          array->list fxarray->list bytearray->list
          array->iter fxarray->iter bytearray->iter
          array->vector fxarray->fxvector bytearray->bytevector
          vector->array fxvector->fxarray bytevector->bytearray)
  (import (cspp chez)
          (cspp internal)
          (cspp list)
          (cspp vector)
          (cspp utils)
          (only (cspp iter) iter-register-source! make-indexed-iter)
          (only (cspp navigator) nav-register-indexed!))

  ;; TODO allow change incr-factor?
  ;; TODO shrink the array when memory is low?

  #|record:$array-storage
  Shared mutable storage used by generic and typed arrays.
  |#
  (define-record-type ($array-storage make-array-storage array-storage?)
    (nongenerative)
    (fields
     ;; the backing vector, whose length is the capacity
     (mutable vec array-vec array-vec-set!)
     (mutable incr-factor array-incr-factor array-incr-factor-set!)
     ;; the actual number of items in vec
     (mutable size $array-size $array-size-set!)))

  #|record:$array
  Mutable generic array with its own public type identity.
  |#
  (define-record-type ($array mk-array array?)
    (nongenerative)
    (parent $array-storage))

  #|record:$fxarray
  Mutable fixnum-array record sharing storage with `$array`.
  |#
  (define-record-type ($fxarray mk-fxarray fxarray?)
    (nongenerative)
    (parent $array-storage))
  #|record:$flarray
  Mutable flonum-array record sharing storage with `$array`.
  |#
  (define-record-type ($flarray mk-flarray flarray?)
    (nongenerative)
    (parent $array-storage))
  #|record:$bytearray
  Mutable unsigned-byte-array record sharing storage with `$array`.
  |#
  (define-record-type ($bytearray mk-bytearray bytearray?)
    (nongenerative)
    (parent $array-storage))

  (define u8? (lambda (x) (and (fixnum? x) (fx<= 0 x 255))))
  ;; default min capacity
  (define *mincap* 64)

  (define all-arrays?   (lambda (x*) (andmap array?   x*)))
  (define all-fxarrays? (lambda (x*) (andmap fxarray? x*)))
  (define all-flarrays? (lambda (x*) (andmap flarray? x*)))
  (define all-bytearrays? (lambda (x*) (andmap bytearray? x*)))


  (define-syntax define-array-procedure
    (lambda (stx)
      (define valid-ty*?
        (lambda (ty*)
          (if (null? (remp (lambda (x) (memq x '(a fxa fla))) ty*))
              #t
              (syntax-error ty* "define-array-procedure: bad array type flags:"))))
      (define handle-ty*
        (lambda (ty*)
          (values (memq 'a ty*) (memq 'fxa ty*) (memq 'fla ty*))))
      (define get-name
        (lambda (which name)
          (let ([n (symbol->string (syntax->datum name))]
                [pre1 '((a . array-) (fxa . fxarray-) (fla . flarray-))])
            ($construct-name name (cdr (assoc which pre1)) n))))
      (syntax-case stx ()
        ;; case-lambda
        [(k (ty* ...) name [args body body* ...] ...)
         (and (identifier? #'name) (valid-ty*? (datum (ty* ...))))
         (let-values ([(pa? pfxa? pfla?) (handle-ty* (datum (ty* ...)))])
           (with-implicit (k v v? vmake vref vset! vcopy vcopy! vlength vpcheck vcheck-length all-which? thisproc who
                             t+ t- t* t/ t+id t*id t> t<
                             a amk amake aadd! a? avec asize avec-set! apcheck aval?)
             #`(begin
                 #,(if pa?
                       (with-syntax ([name (get-name 'a #'name)])
                         #`(module (name)
                             (define a         array)
                             (define amk       mk-array)
                             (define amake     make-array)
                             (define aadd!     array-add!)
                             (define a?        array?)
                             (define avec      array-vec)
                             (define avec-set! array-vec-set!)
                             (define asize   $array-size)
                             (define aval?     (lambda (x) #t))
                             (define v     vector)
                             (define v?    vector?)
                             (define vmake make-vector)
                             (define vref  vector-ref)
                             (define vset! vector-set!)
                             (define vlength vector-length)
                             (define vcopy!  vector-copy!)
                             (define vcopy   vector-copy)
                             ;;(define vcheck-length check-length)
                             (define all-which? all-arrays?)
                             (define who 'name)
                             (define t+ +)   (define t- -)
                             (define t* *)   (define t/ /)
                             (define t+id 0) (define t*id 1)
                             (define t> >)   (define t< <)
                             (let-syntax ([vpcheck (syntax-rules () [(_ e* (... ...)) (pcheck-vector e* (... ...))])]
                                          [apcheck (syntax-rules () [(_ (a* (... ...)) e* (... ...)) (pcheck ([array? a* (... ...)]) e* (... ...))])])
                               (define name
                                 (case-lambda
                                   [args body body* ...] ...))
                               (define thisproc name))))
                       ;; to create a proper definition context
                       #'(define dummy0 'dummy))
                 #,(if pfxa?
                       (with-syntax ([name (get-name 'fxa #'name)])
                         #`(module (name)
                             (define a         fxarray)
                             (define amk       mk-fxarray)
                             (define amake     make-fxarray)
                             (define aadd!     fxarray-add!)
                             (define a?        fxarray?)
                             (define avec      array-vec)
                             (define avec-set! array-vec-set!)
                             (define asize   $array-size)
                             (define aval?     (lambda (x) (unless (fixnum? x) (errorf 'name "not a fixnum: ~a" x))))
                             (define v     fxvector)
                             (define v?    fxvector?)
                             (define vmake make-fxvector)
                             (define vref  fxvector-ref)
                             (define vset! fxvector-set!)
                             (define vlength fxvector-length)
                             (define vcopy!  fxvcopy!)
                             (define vcopy   fxvector-copy)
                             ;;(define vcheck-length check-fxlength)
                             (define all-which? all-fxarrays?)
                             (define who 'name)
                             (define t+ fx+)
                             (define t- fx-)
                             (define t* fx*)
                             (define t/ fx/)
                             (define t+id 0)
                             (define t*id 1)
                             (define t> fx>)   (define t< fx<)
                             (let-syntax ([vpcheck (syntax-rules () [(_ e* (... ...)) (pcheck-fxvector e* (... ...))])]
                                          [apcheck (syntax-rules () [(_ (a* (... ...)) e* (... ...)) (pcheck ([fxarray? a* (... ...)]) e* (... ...))])])
                               (define name
                                 (case-lambda
                                   [args body body* ...] ...))
                               (define thisproc name))))
                       #'(define dummy1 'dummy))
                 #,(if pfla?
                       (with-syntax ([name (get-name 'fla #'name)])
                         #`(module (name)
                             (define a flarray)
                             (define amk mk-flarray)
                             (define amake make-flarray)
                             (define aadd! flarray-add!)
                             (define a? flarray?)
                             (define avec array-vec)
                             (define avec-set! array-vec-set!)
                             (define asize $array-size)
                             (define aval?
                               (lambda (x)
                                 (unless (flonum? x)
                                   (errorf 'name "not a flonum: ~a" x))))
                             (define v flvector)
                             (define v? flvector?)
                             (define vmake make-flvector)
                             (define vref flvector-ref)
                             (define vset! flvector-set!)
                             (define vlength flvector-length)
                             (define vcopy! flvcopy!)
                             (define vcopy flvector-copy)
                             (define all-which? all-flarrays?)
                             (define who 'name)
                             (define t+ fl+) (define t- fl-)
                             (define t* fl*) (define t/ fl/)
                             (define t+id 0.0) (define t*id 1.0)
                             (define t> fl>) (define t< fl<)
                             (let-syntax
                                 ([vpcheck
                                   (syntax-rules ()
                                     [(_ e* (... ...))
                                      (pcheck-flvector e* (... ...))])]
                                  [apcheck
                                   (syntax-rules ()
                                     [(_ (a* (... ...)) e* (... ...))
                                      (pcheck ([flarray? a* (... ...)]) e* (... ...))])])
                               (define name
                                 (case-lambda
                                   [args body body* ...] ...))
                               (define thisproc name))))
                       #'(define dummy-fl 'dummy))
                 )))]
        ;; lambda
        [(k (ty* ...) (name . args) body* ...)
         (and (identifier? #'name) (valid-ty*? (datum (ty* ...))))
         (let-values ([(pa? pfxa? pfla?) (handle-ty* (datum (ty* ...)))])
           (with-implicit (k v? vmake vref vset! vlength vcopy vcopy! vpcheck vcheck-length all-which? thisproc who
                             t+ t- t* t/ t+id t*id t> t<
                             a amk amake aadd! a? avec asize avec-set! apcheck aval?)
             #`(begin
                 #,(if pa?
                       (with-syntax ([name (get-name 'a #'name)])
                         #`(define name
                             (lambda args
                               (let ([a         array]
                                     [amk       mk-array]
                                     [amake     make-array]
                                     [aadd!     array-add!]
                                     [a?        array?]
                                     [avec      array-vec]
                                     [avec-set! array-vec-set!]
                                     [asize   $array-size]
                                     ;; check value type
                                     [aval?     (lambda (x) #t)]
                                     [v     vector]
                                     [v?    vector?]
                                     [vmake make-vector]
                                     [vref  vector-ref]
                                     [vset! vector-set!]
                                     [vlength vector-length]
                                     [vcopy!  vector-copy!]
                                     [vcopy   vector-copy]
                                     ;;[vcheck-length check-length]
                                     [all-which? all-arrays?]
                                     [who 'name]
                                     [thisproc name]
                                     [t+ +] [t- -] [t* *] [t/ /] [t+id 0] [t*id 1]
                                     [t> >] [t< <])
                                 ;; this piece of syntax needs care
                                 (let-syntax ([vpcheck (syntax-rules () [(_ e* (... ...)) (pcheck-vector e* (... ...))])]
                                              [apcheck (syntax-rules () [(_ (a* (... ...)) e* (... ...)) (pcheck ([array? a* (... ...)]) e* (... ...))])])
                                   body* ...)))))
                       ;; to create a proper definition context
                       #'(define dummy0 'dummy))
                 #,(if pfxa?
                       (with-syntax ([name (get-name 'fxa #'name)])
                         #`(define name
                             (lambda args
                               (let ([a         fxarray]
                                     [amk       mk-fxarray]
                                     [amake     make-fxarray]
                                     [aadd!     fxarray-add!]
                                     [a?        fxarray?]
                                     [avec      array-vec]
                                     [avec-set! array-vec-set!]
                                     [asize   $array-size]
                                     [aval? (lambda (x) (unless (fixnum? x) (errorf 'name "not a fixnum: ~a" x)))]
                                     [v     fxvector]
                                     [v?    fxvector?]
                                     [vmake make-fxvector]
                                     [vref  fxvector-ref]
                                     [vset! fxvector-set!]
                                     [vlength fxvector-length]
                                     [vcopy!  fxvcopy!]
                                     [vcopy   fxvector-copy]
                                     ;;[vcheck-length check-fxlength]
                                     [all-which? all-fxarrays?]
                                     [who 'name]
                                     [thisproc name]
                                     [t+ fx+] [t- fx-] [t* fx*] [t/ fx/] [t+id 0] [t*id 1]
                                     [t> fx>] [t< fx<])
                                 (let-syntax ([vpcheck (syntax-rules () [(_ e* (... ...)) (pcheck-fxvector e* (... ...))])]
                                              [apcheck (syntax-rules () [(_ (a* (... ...)) e* (... ...)) (pcheck ([fxarray? a* (... ...)]) e* (... ...))])])
                                   body* ...)))))
                       #'(define dummy1 'dummy))
                 #,(if pfla?
                       (with-syntax ([name (get-name 'fla #'name)])
                         #`(define name
                             (lambda args
                               (let ([a flarray]
                                     [amk mk-flarray]
                                     [amake make-flarray]
                                     [aadd! flarray-add!]
                                     [a? flarray?]
                                     [avec array-vec]
                                     [avec-set! array-vec-set!]
                                     [asize $array-size]
                                     [aval? (lambda (x)
                                              (unless (flonum? x)
                                                (errorf 'name "not a flonum: ~a" x)))]
                                     [v flvector]
                                     [v? flvector?]
                                     [vmake make-flvector]
                                     [vref flvector-ref]
                                     [vset! flvector-set!]
                                     [vlength flvector-length]
                                     [vcopy! flvcopy!]
                                     [vcopy flvector-copy]
                                     [all-which? all-flarrays?]
                                     [who 'name]
                                     [thisproc name]
                                     [t+ fl+] [t- fl-] [t* fl*] [t/ fl/]
                                     [t+id 0.0] [t*id 1.0]
                                     [t> fl>] [t< fl<])
                                 (let-syntax
                                     ([vpcheck
                                       (syntax-rules ()
                                         [(_ e* (... ...))
                                          (pcheck-flvector e* (... ...))])]
                                      [apcheck
                                       (syntax-rules ()
                                         [(_ (a* (... ...)) e* (... ...))
                                          (pcheck ([flarray? a* (... ...)]) e* (... ...))])])
                                   body* ...)))))
                       #'(define dummy-fl 'dummy))
                 )))])))


  (define $make-array
    (lambda (who cap v len)
      (pcheck ([natural? cap len])
              (mk-array (make-vector cap v) 2 len))))
  (define $make-fxarray
    (lambda (who cap v len)
      (pcheck ([natural? cap len] [fixnum? v])
              (mk-fxarray (make-fxvector cap v) 2 len))))
  (define $make-flarray
    (lambda (who cap v len)
      (pcheck ([natural? cap len] [flonum? v])
              (mk-flarray (make-flvector cap v) 2 len))))
  (define $make-bytearray
    (lambda (who cap v len)
      (pcheck ([natural? cap len] [u8? v])
              (mk-bytearray (make-bytevector cap v) 2 len))))


  #|proc:make-array
  Create an array.

  If no arguments are given, an empty array is created.
  If `len` is given, an array with `len` items all set to #f is returned.
  If both `len` and `v` are given, an array with `len` items all set to `v` is returned.

  The other types of array makers have similar semantics, with the exception that
  if only `len` is given, the items are set to 0 by default.
  |#
  (define-who make-array
    (case-lambda
      [()      ($make-array who *mincap* #f 0)]
      [(len)   ($make-array who len      #f len)]
      [(len v) ($make-array who len      v  len)]))

  #|proc:make-fxarray
  Return a fixnum array of optional length `len`, filled with optional fixnum `v`.
  |#
  (define-who make-fxarray
    (case-lambda
      [()      ($make-fxarray who *mincap* #f 0)]
      [(len)   ($make-fxarray who len      0  len)]
      [(len v) ($make-fxarray who len      v  len)]))

  #|proc:make-flarray
  Return a flonum array of optional length `len`, filled with optional flonum `v`.
  |#
  (define-who make-flarray
    (case-lambda
      [() ($make-flarray who *mincap* 0.0 0)]
      [(len) ($make-flarray who len 0.0 len)]
      [(len v) ($make-flarray who len v len)]))

  #|proc:make-bytearray
  Return a byte array of optional length `len`, filled with optional byte `v`.
  |#
  (define-who make-bytearray
    (case-lambda
      [()      ($make-bytearray who *mincap* #f 0)]
      [(len)   ($make-bytearray who len      0  len)]
      [(len v) ($make-bytearray who len      v  len)]))


  #|proc:array
  Create an array from the given arguments.
  |#
  (define-who array
    (lambda args
      (let ([len (length args)])
        (let* ([arr (make-array (if (fx< len *mincap*) *mincap* len))] [vec (array-vec arr)])
          (let loop ([i 0] [args args])
            (if (null? args)
                (begin ($array-size-set! arr len)
                       arr)
                (begin (vector-set! vec i (car args))
                       (loop (fx1+ i) (cdr args)))))))))

  #|proc:fxarray
  Return a fixnum array containing `args` in argument order.
  |#
  (define-who fxarray
    (lambda args
      (unless (andmap fixnum? args)
        (errorf who "arguments must be fixnums: ~a" args))
      (let ([len (length args)])
        (let* ([arr (make-fxarray (if (fx< len *mincap*) *mincap* len))] [vec (array-vec arr)])
          (let loop ([i 0] [args args])
            (if (null? args)
                (begin ($array-size-set! arr len)
                       arr)
                (begin (fxvector-set! vec i (car args))
                       (loop (fx1+ i) (cdr args)))))))))

  #|proc:flarray
  Return a flonum array containing `args` in argument order.
  |#
  (define-who flarray
    (lambda args
      (unless (andmap flonum? args) (errorf who "arguments must be flonums: ~a" args))
      (let* ([len (length args)] [arr (make-flarray (max *mincap* len))]
             [vec (array-vec arr)])
        (let loop ([i 0] [xs args])
          (if (null? xs)
              (begin ($array-size-set! arr len) arr)
              (begin (flvector-set! vec i (car xs)) (loop (fx1+ i) (cdr xs))))))))

  #|proc:flarray->flvector
  Convert `arr` to an exact-size flvector.
  |#
  (define-who flarray->flvector
    (lambda (arr) (pcheck ([flarray? arr])
                          (let ([v (make-flvector ($array-size arr) 0.0)])
                            (flvcopy! (array-vec arr) 0 v 0 ($array-size arr)) v))))
  #|proc:flvector->flarray
  Convert flvector `vec` to a flarray.
  |#
  (define-who flvector->flarray
    (lambda (vec) (pcheck-flvector (vec)
                                   (let* ([n (flvector-length vec)] [arr (make-flarray n)])
                                     (flvcopy! vec 0 (array-vec arr) 0 n)
                                     arr))))

  #|proc:bytearray
  Return a byte array containing `args` in argument order.
  |#
  (define-who bytearray
    (lambda args
      (unless (andmap u8? args)
        (errorf who "arguments must be bytes: ~a" args))
      (let ([len (length args)])
        (let* ([arr (make-bytearray (if (fx< len *mincap*) *mincap* len))] [vec (array-vec arr)])
          (let loop ([i 0] [args args])
            (if (null? args)
                (begin ($array-size-set! arr len)
                       arr)
                (begin (bytevector-u8-set! vec i (car args))
                       (loop (fx1+ i) (cdr args)))))))))


  #|proc:array-size
  Return the number of items in the generic array `arr`.
  |#
  #|proc:fxarray-size
  Return the number of items in the fxarray.
  |#
  #|proc:flarray-size
  Return the number of items in flarray `arr`.
  |#
  (define-array-procedure (a fxa fla)
    (size arr)
    (apcheck (arr)
             ($array-size arr)))


  #|proc:list->array
  Return whether the array is empty.
  |#
  (define-array-procedure (a fxa fla)
    (empty? arr)
    (apcheck (arr)
             (fx= 0 ($array-size arr))))


  (define $grow-array!
    (lambda (arr)
      (let* ([len ($array-size arr)] [vec (array-vec arr)]
             [vmake (cond [(vector?     vec) make-vector]
                          [(fxvector?   vec) make-fxvector]
                          [(flvector?   vec) make-flvector]
                          [(bytevector? vec) make-bytevector]
                          [else (assert-unreachable)])]
             [vlength (cond [(vector?     vec) vector-length]
                            [(fxvector?   vec) fxvector-length]
                            [(flvector?   vec) flvector-length]
                            [(bytevector? vec) bytevector-length]
                            [else (assert-unreachable)])]
             [vcopy! (cond [(vector?     vec) vcopy!]
                           [(fxvector?   vec) fxvcopy!]
                           [(flvector?   vec) flvcopy!]
                           [(bytevector? vec) bytevector-copy!]
                           [else (assert-unreachable)])]
             [cap (vlength vec)]
             [fill (if (flvector? vec) 0.0 0)]
             [newvec (vmake (fx* (if (fx= cap 0) *mincap* cap)
                                  (array-incr-factor arr))
                            fill)])
        ;;(printf "growing array from ~a to ~a~n" cap (vlength newvec))
        (vcopy! vec 0 newvec 0 len)
        (array-vec-set! arr newvec))))


  #|doc
  Add a value either to the end of the array `arr` or at a specified index.
  |#
  ;; this is used in `define-array-procdure`, so need to defined separately
  (define-syntax define-array-add!
    (syntax-rules ()
      [(_ thisproc a? aval? vlength vset! vcopy!)
       (define thisproc
         (case-lambda
           [(arr v) (pcheck ([a? arr]) (thisproc arr ($array-size arr) v))]
           [(arr i v)
            (pcheck ([a? arr] [natural? i] [aval? v])
                    (let* ([len ($array-size arr)] [vec (array-vec arr)] [cap (vlength vec)])
                      (when (fx> i len) (errorf 'thisproc "index ~a out of range ~a" i len))
                      (when (fx= len cap) ($grow-array! arr))
                      (when (fx< i len) (vcopy! (array-vec arr) i (array-vec arr) (fx1+ i) (fx- len i)))
                      (vset! (array-vec arr) i v)
                      ($array-size-set! arr (fx1+ len))))]))]))

  (define-array-add! array-add!   array?   (lambda (x) #t) vector-length     vector-set!        vcopy!)
  (define-array-add! fxarray-add! fxarray? fixnum?         fxvector-length   fxvector-set!      fxvcopy!)
  (define-array-add! flarray-add! flarray? flonum?         flvector-length   flvector-set!      flvcopy!)

  (define $array-add-values!
    (lambda (who arr i values value-check vector-make vector-length vector-set vector-copy! fill)
      (let* ([len ($array-size arr)] [count (length values)] [newlen (fx+ len count)]
             [old (array-vec arr)] [capacity (vector-length old)])
        (when (fx> i len)
          (errorf who "index ~a out of range ~a" i len))
        (for-each value-check values)
        (if (fx>= capacity newlen)
            (when (fx< i len)
              (vector-copy! old i old (fx+ i count) (fx- len i)))
            (let capacity-loop ([new-capacity (if (fx= capacity 0) *mincap* capacity)])
              (if (fx>= new-capacity newlen)
                  (let ([new (vector-make new-capacity fill)])
                    (vector-copy! old 0 new 0 i)
                    (when (fx< i len)
                      (vector-copy! old i new (fx+ i count) (fx- len i)))
                    (array-vec-set! arr new))
                  (capacity-loop (fx* new-capacity (array-incr-factor arr))))))
        (let ([vec (array-vec arr)])
          (let write ([j i] [rest values])
            (unless (null? rest)
              (vector-set vec j (car rest))
              (write (fx1+ j) (cdr rest)))))
        ($array-size-set! arr newlen))))


  #|doc
  Add multiple values either to the end of the array `arr` or at a specified index.
  This is faster than `array-add!` when adding multiple values.
  |#
  (define-array-procedure (a fxa fla) add*!
    [(arr) (apcheck (arr) arr)]
    [(arr first . rest)
     (apcheck (arr)
              (if (and (pair? rest) (fixnum? first) (fx>= first 0)
                       (fx<= first (asize arr)))
                  ($array-add-values! who arr first rest aval?
                                      vmake vlength vset! vcopy! t+id)
                  ($array-add-values! who arr (asize arr) (cons first rest) aval?
                                      vmake vlength vset! vcopy! t+id)))])


  #|doc
  Return the value at the specified index.
  TODO default value?
  |#
  (define-array-procedure (a fxa fla)
    (ref arr i)
    (apcheck (arr)
             (pcheck ([natural? i])
                     (let* ([len (asize arr)] [vec (array-vec arr)])
                       (if (and (fx<= 0 i) (fx< i len))
                           (vref vec i)
                           (errorf who "index ~a out of range ~a" i len))))))


  #|doc
  Update the value in the array at the specified index.
  |#
  (define-array-procedure (a fxa fla)
    (set! arr i v)
    (apcheck (arr)
             (pcheck ([natural? i])
                     (aval? v)
                     (let* ([len (asize arr)] [vec (array-vec arr)])
                       (if (and (fx<= 0 i) (fx< i len))
                           (vset! vec i v)
                           (errorf who "index ~a out of range ~a" i len))))))


  #|doc
  Delete the value at the specified index.
  |#
  (define-array-procedure (a fxa fla)
    (delete! arr i)
    (apcheck (arr)
             (pcheck ([natural? i])
                     (let ([len ($array-size arr)] [vec (array-vec arr)])
                       (when (fx>= i len) (errorf who "index ~a out of range ~a" i len))
                       (cond [(fx= i 0) (vcopy! vec 1 vec 0 (fx- len 1))]
                             [(fx= i (fx1- len)) (void)]
                             [else (vcopy! vec (fx1+ i) vec i (fx- len i 1))])
                       ($array-size-set! arr (fx1- len))))))

  ;; TODO delete in range


  #|doc
  Remove all items in the array.
  |#
  (define-array-procedure (a fxa fla)
    (clear! arr)
    (apcheck (arr)
             ;; just set length to 0 for now
             ($array-size-set! arr 0)))


  #|doc
  Apply `pred` to every item of the array `arr` and return a new array
  of the items of `arr` for which `pred` returns #t.
  |#
  (define-array-procedure (a fxa fla)
    (filter pred arr)
    (apcheck (arr)
             (pcheck ([procedure? pred])
                     (let ([newarr (amake)] [len ($array-size arr)] [vec (array-vec arr)])
                       (let loop ([i 0])
                         (if (fx= i len)
                             newarr
                             (let ([v (vref vec i)])
                               (when (pred v) (aadd! newarr v))
                               (loop (fx1+ i)))))))))


  #|doc
  Similar to `array-filter`, but array `arr` is modified in place to contain
  only items `x` such that `(pred x)` returns #t.
  |#
  (define-array-procedure (a fxa fla)
    (filter! pred arr)
    (apcheck (arr)
             (pcheck ([procedure? pred])
                     ;; This may incur memory waste when the remaining items are few...
                     (let ([len ($array-size arr)] [vec (array-vec arr)])
                       ;; i: store index, j: scan index
                       (let loop ([i 0] [j 0])
                         (if (fx= j len)
                             ($array-size-set! arr i)
                             (let ([v (vref vec j)])
                               (if (pred v)
                                   (begin (unless (fx= i j)
                                            (vset! vec i v))
                                          (loop (fx1+ i) (fx1+ j)))
                                   (loop i (fx1+ j))))))))))


  #|doc
  Return two arrays, the first array contains values `x` such that `(proc x)` returns #t,
  the second contains values `x` such that `(proc x)` returns #f.
  |#
  (define-array-procedure (a fxa fla)
    (partition proc arr)
    (apcheck (arr)
             (pcheck ([procedure? proc])
                     (let ([T (amake)] [F (amake)] [len ($array-size arr)] [vec (array-vec arr)])
                       (let loop ([i 0])
                         (if (fx= i len)
                             (values T F)
                             (let ([v (vref vec i)])
                               (if (proc v)
                                   (aadd! T v)
                                   (aadd! F v))
                               (loop (fx1+ i)))))))))


  #|doc
  Return a new array whose items are those from the given array, in the given order.
  |#
  (define-array-procedure (a fxa fla)
    (append arr . arr*)
    (apcheck (arr)
             (pcheck ([all-which? arr*])
                     (let* ([arr* (cons arr arr*)]
                            [len (apply fx+ (map $array-size arr*))]
                            [newarr (amake len)] [newvec (array-vec newarr)])
                       ($array-size-set! newarr len)
                       (let next ([i 0] [arr* arr*])
                         (if (null? arr*)
                             newarr
                             (let* ([arr (car arr*)] [vec (array-vec arr)] [len ($array-size arr)])
                               (let loop ([i i] [j 0])
                                 (if (fx= j len)
                                     (next i (cdr arr*))
                                     (begin (vset! newvec i (vref vec j))
                                            (loop (fx1+ i) (fx1+ j))))))))))))


  #|doc
  Imperatively append items of given arrays `arr*` to array `arr`,
  then return the first array `arr`.
  |#
  (define-array-procedure (a fxa fla)
    (append! arr . arr*)
    (apcheck (arr)
             (unless (null? arr*)
               (pcheck ([all-which? arr*])
                       (let* ([len1 ($array-size arr)] [vec1 (array-vec arr)] [cap1 (vlength vec1)]
                              [len* (apply fx+ (map $array-size arr*))]
                              [fillvec! (lambda (tgtvec i arr*)
                                          (let next ([i i] [arr* arr*])
                                            (unless (null? arr*)
                                              (let* ([arr (car arr*)]
                                                     [vec (array-vec arr)] [len ($array-size arr)])
                                                (let loop ([i i] [j 0])
                                                  (if (fx= j len)
                                                      (next i (cdr arr*))
                                                      (begin (vset! tgtvec i (vref vec j))
                                                             (loop (fx1+ i) (fx1+ j)))))))))])
                         (if (fx<= len* (fx- cap1 len1))
                             (fillvec! vec1 len1 arr*)
                             (let ([newvec (vmake (fx+ len1 len*))])
                               (fillvec! newvec 0 (cons arr arr*))
                               (array-vec-set! arr newvec)))
                         ($array-size-set! arr (fx+ len1 len*))
                         arr)))))



  #|doc
  Return a newly allocated array consisting of the items of `arr` in reverse order.
  |#
  (define-array-procedure (a fxa fla)
    (reverse arr)
    (apcheck (arr)
             (let* ([len ($array-size arr)] [vec (array-vec arr)]
                    [newarr (amake len)]     [newvec (array-vec newarr)])
               (let loop ([i 0] [j (fx1- len)])
                 (unless (fx= i len)
                   (vset! newvec j (vref vec i))
                   (loop (fx1+ i) (fx1- j))))
               ($array-size-set! newarr len)
               newarr)))


  #|doc
  Reverse the items in the array in place, then return the array.
  |#
  (define-array-procedure (a fxa fla)
    (reverse! arr)
    (apcheck (arr)
             (let ([len ($array-size arr)] [vec (array-vec arr)])
               (let loop ([i 0] [j (fx1- len)])
                 (when (fx< i j)
                   (let ([x (vref vec i)] [y (vref vec j)])
                     (vset! vec i y)
                     (vset! vec j x)
                     (loop (fx1+ i) (fx1- j)))))
               arr)))


  #|proc:array-contains?
  Return whether the array contains the given item using `equal?`.
  |#
  #|proc:fxarray-contains?
  Return whether the fxarray contains the given item using `equal?`.
  |#
  #|proc:bytearray-contains?
  Return whether the array contains the given item.
  Items are compared using `equal?`.
  |#
  (define-array-procedure (a fxa fla)
    (contains? arr v)
    (apcheck (arr)
             (aval? v)
             (let ([len ($array-size arr)] [vec (array-vec arr)])
               (let loop ([i 0])
                 (if (fx= i len)
                     #f
                     (if (equal? v (vref vec i))
                         #t
                         (loop (fx1+ i))))))))


  #|proc:array-contains/p?
  Return whether the array contains an item that satisfies `pred`.
  |#
  #|proc:fxarray-contains/p?
  Return whether the fxarray contains an item that satisfies `pred`.
  |#
  #|proc:bytearray-contains/p?
  Return whether the array contains an item that satisfies `pred`.
  |#
  (define-array-procedure (a fxa fla)
    (contains/p? arr pred)
    (apcheck (arr)
             (pcheck ([procedure? pred])
                     (let ([len ($array-size arr)] [vec (array-vec arr)])
                       (let loop ([i 0])
                         (if (fx= i len)
                             #f
                             (if (pred (vref vec i))
                                 #t
                                 (loop (fx1+ i)))))))))


  #|proc:array-index-of
  Return the index of the first item equal to `v`, or #f if no item matches.
  |#
  #|proc:fxarray-index-of
  Return the index of the first item equal to `v`, or #f if no item matches.
  |#
  #|proc:bytearray-index-of
  Return the index of the first item equal to `v`, or #f if no item matches.
  |#
  (define-array-procedure (a fxa fla)
    (index-of arr v)
    (apcheck (arr)
             (aval? v)
             (let ([len ($array-size arr)] [vec (array-vec arr)])
               (let loop ([i 0])
                 (if (fx= i len)
                     #f
                     (if (equal? v (vref vec i))
                         i
                         (loop (fx1+ i))))))))


  #|proc:array-find-index
  Return the index of the first item satisfying `pred`, or #f if no item matches.
  |#
  #|proc:fxarray-find-index
  Return the index of the first item satisfying `pred`, or #f if no item matches.
  |#
  #|proc:bytearray-find-index
  Return the index of the first item satisfying `pred`, or #f if no item matches.
  |#
  (define-array-procedure (a fxa fla)
    (find-index arr pred)
    (apcheck (arr)
             (pcheck ([procedure? pred])
                     (let ([len ($array-size arr)] [vec (array-vec arr)])
                       (let loop ([i 0])
                         (if (fx= i len)
                             #f
                             (if (pred (vref vec i))
                                 i
                                 (loop (fx1+ i)))))))))


  #|proc:array-search
  Return the first item satisfying `pred`, or `default` if no item matches.
  `default` defaults to #f; supply a unique value when #f is a valid item.
  |#
  #|proc:fxarray-search
  Return the first item satisfying `pred`, or `default` if no item matches.
  |#
  #|proc:bytearray-search
  Return the first item in the array that satisfies the predicate `pred`.
  If no such item is found, `default` is returned.
  |#
  (define-array-procedure (a fxa fla) search
    [(arr pred) (thisproc arr pred #f)]
    [(arr pred default)
     (apcheck (arr)
              (pcheck ([procedure? pred])
                      (let ([len ($array-size arr)] [vec (array-vec arr)])
                        (let loop ([i 0])
                          (if (fx= i len)
                              default
                              (let ([v (vref vec i)])
                                (if (pred v) v (loop (fx1+ i)))))))))])


  #|doc
  Search for items in array `dl` that satisfies the predicate `pred`.

  By default the items satisfying `pred` are returned in a list.

  The `collect` argument has the same semantics as in `dlist-search*`.
  |#
  (define-array-procedure (a fxa fla) search*
    [(arr pred)
     (apcheck (arr)
              (pcheck ([procedure? pred])
                      (let ([lb (make-list-builder)])
                        (thisproc arr pred (lambda (x) (lb x)))
                        (lb))))]
    [(arr pred collect)
     (apcheck (arr)
              (pcheck ([procedure? pred collect])
                      (let ([len ($array-size arr)] [vec (array-vec arr)])
                        (let loop ([i 0])
                          (unless (fx= i len)
                            (let ([v (vref vec i)])
                              (when (pred v) (collect v))
                              (loop (fx1+ i))))))))])


  #|doc
  Return a slice (sub-array) of the array `arr` specified by `start`, `end` and `step`.

  Meanings of `start`, `end` and `step` are the same as in list:slice.

  If the indices are out of range in any way, an empty array is returned.
  |#
  (define-array-procedure (a fxa fla) slice
    [(arr end) (thisproc arr 0 end 1)]
    [(arr start end) (thisproc arr start end 1)]
    [(arr start end step)
     (pcheck ([a? arr] [fixnum? start end step])
             (when (fx= step 0) (errorf who "step cannot be 0"))
             (let* ([vec (array-vec arr)] [len ($array-size arr)]
                    [s (let ([s (if (fx>= start 0) start (fx+ len start))])
                         (cond [(fx< s 0) 0]
                               [(fx> s len) (fx1- len)]
                               [else s]))]
                    [e (let ([e (if (fx>= end 0) end (fx+ len end))])
                         (cond [(fx<= e -1) -1]
                               [(fx>= e len) len]
                               [else e]))])
               (if (fx= len 0)
                   (amake 0)
                   (let ([newv (cond [(and (fx< s e) (fx> step 0))
                                      (let ([newv (vmake (ceiling (/ (fx- e s) step)))])
                                        ;; forward
                                        (let loop ([s s] [i 0])
                                          (if (fx>= s e)
                                              newv
                                              (begin (vset! newv i (vref vec s))
                                                     (loop (fx+ s step) (fx1+ i))))))]
                                     [(and (fx> s e) (fx< step 0))
                                      (let ([newv (vmake (ceiling (/ (fx- e s) step)))])
                                        ;; backward
                                        (let loop ([s s] [i 0])
                                          (if (fx<= s e)
                                              newv
                                              (begin (vset! newv i (vref vec s))
                                                     (loop (fx+ s step) (fx1+ i))))))]
                                     [else (vmake 0)])])
                     (amk newv (array-incr-factor arr) (vlength newv))))))])


  #|doc
  Imperatively slice the array `arr` to the range specified by `start`, `end` and `step`.

  Meanings of `start`, `end` and `step` are the same as in list:slice.

  If the indices are out of range in any way, this procedure has no effect on the array.

  After the operation, `arr` is returned.
  |#
  (define-array-procedure (a fxa fla) slice!
    [(arr end) (thisproc arr 0 end 1)]
    [(arr start end) (thisproc arr start end 1)]
    [(arr start end step)
     (pcheck ([a? arr] [fixnum? start end step])
             (when (fx= step 0) (errorf who "step cannot be 0"))
             (let* ([vec (array-vec arr)] [len ($array-size arr)]
                    [s (let ([s (if (fx>= start 0) start (fx+ len start))])
                         (cond [(fx< s 0) 0]
                               [(fx> s len) (fx1- len)]
                               [else s]))]
                    [e (let ([e (if (fx>= end 0) end (fx+ len end))])
                         (cond [(fx<= e -1) -1]
                               [(fx>= e len) len]
                               [else e]))])
               (when (fx> len 0)
                 ;; TODO try to reuse `vec`
                 (let ([newv (cond [(and (fx< s e) (fx> step 0))
                                    (let ([newv (vmake (ceiling (/ (fx- e s) step)))])
                                      ;; forward
                                      (let loop ([s s] [i 0])
                                        (if (fx>= s e)
                                            newv
                                            (begin (vset! newv i (vref vec s))
                                                   (loop (fx+ s step) (fx1+ i))))))]
                                   [(and (fx> s e) (fx< step 0))
                                    (let ([newv (vmake (ceiling (/ (fx- e s) step)))])
                                      ;; backward
                                      (let loop ([s s] [i 0])
                                        (if (fx<= s e)
                                            newv
                                            (begin (vset! newv i (vref vec s))
                                                   (loop (fx+ s step) (fx1+ i))))))]
                                   [else #f])])
                   (when newv
                     (array-vec-set!    arr newv)
                     ($array-size-set! arr (vlength newv)))))
               arr))])


  #|doc
  Make a copy of the array `arr`.
  |#
  (define-array-procedure (a fxa fla)
    (copy arr)
    (apcheck (arr)
             (amk (vcopy (array-vec arr))
                  (array-incr-factor arr)
                  ($array-size arr))))


  #|doc
  Copy items in `src` from indices src-start, ..., src-start + k - 1
  to consecutive indices in `tgt` starting at `tgt-start`.

  `src` and `tgt` must be arrays of the same type.
  `src-start`, `tgt-start`, and `k` must be exact nonnegative integers.
  The sum of `src-start` and `k` must not exceed the length of `src`,
  and the sum of `tgt-start` and `k` must not exceed the length of `tgt`.

  `src` and `tgt` may or may not be the same array.
  |#
  (define-array-procedure (a fxa fla)
    (copy! src src-start tgt tgt-start k)
    (apcheck (src tgt)
             (pcheck ([natural? src-start tgt-start k])
                     (let ([len1 ($array-size src)] [vec1 (array-vec src)]
                           [len2 ($array-size tgt)] [vec2 (array-vec tgt)])
                       (when (> (fx+ src-start k) len1)
                         (errorf who "range ~a is too large in source array" k))
                       (when (> (fx+ tgt-start k) len2)
                         (errorf who "range ~a is too large in target array" k))
                       (when (fx> k 0)
                         (if (eq? src tgt)
                             (let ([src-end (fx+ src-start k)] [tgt-end (fx+ tgt-start k)])
                               (cond
                                [(or
                                  ;; disjoint, left to right
                                  (fx<= src-end tgt-start)
                                  ;; disjoint, right to left
                                  (fx<= tgt-end src-start)
                                  ;; overlapping, right to left
                                  (fx<= tgt-start src-start))
                                 (let loop ([i src-start] [j tgt-start] [k k])
                                   (unless (fx= k 0)
                                     (vset! vec2 j (vref vec1 i))
                                     (loop (fx1+ i) (fx1+ j) (fx1- k))))]
                                [(fx< src-start tgt-start)
                                 ;; overlapping, left to right, copy from last to first
                                 (let loop ([i (fx1- src-end)] [j (fx1- tgt-end)] [k k])
                                   (unless (fx= k 0)
                                     (vset! vec2 j (vref vec1 i))
                                     (loop (fx1- i) (fx1- j) (fx1- k))))]
                                [else (assert-unreachable)]))
                             (let loop ([i src-start] [j tgt-start] [k k])
                               (unless (fx= k 0)
                                 (vset! vec2 j (vref vec1 i))
                                 (loop (fx1+ i) (fx1+ j) (fx1- k))))))))))


  (define $sorted?
    (lambda (vec <? start stop vref)
      (let loop ([i start])
        (if (fx= i (fx1- stop))
            #t
            (and (<? (vref vec i) (vref vec (fx1+ i)))
                 (loop (fx1+ i)))))))

  #|doc
  Check whether the array is sorted according to the comparison procedure `<?`.
  If `stop` is given, only the items with indices [0, stop) are checked;
  If both `start` and `stop` are given, only the items with indices [start, stop) are checked.

  `start` and `stop` must satisfy the requirement that `0 <= start <= stop <= length of array`.
  |#
  (define-array-procedure (a fxa fla) sorted?
    [(<? arr)
     (apcheck (arr)
              (thisproc <? arr 0 (asize arr)))]
    [(<? arr stop)
     (apcheck (arr)
              (thisproc <? arr 0 stop))]
    [(<? arr start stop)
     (apcheck (arr)
              (pcheck ([procedure? <?] [natural? start stop])
                      (let ([len ($array-size arr)] [vec (array-vec arr)])
                        (when (fx> stop len)
                          (errorf who "stop index ~a out of bound ~a" stop len))
                        (when (fx> start stop)
                          (errorf who "start index ~a greater than stop index ~a" start stop))
                        (if (fx<= len 1)
                            #t
                            ($sorted? vec <? start stop vref)))))])


  #|doc
  The `*array-sort` procedures use the binary comparison procedure `<?` to sort the array `arr`.
  If only two arguments are given, the entire array is sorted;
  If the `stop` argument is given, the range from 0 to `stop-1` in `arr` is sorted;
  If both `start` and `stop` are given, the range from `start` to `stop-1` in `arr` is sorted.

  `start` and `stop` must satisfy the requirement that `0 <= start <= stop <= length of arr`.

  The `*array-sort` procedures return the sorted array or the subarray.
  |#
  (define-array-procedure (a fxa fla) sort
    [(<? arr)
     (apcheck (arr)
              (thisproc <? arr 0 (asize arr)))]
    [(<? arr stop)
     (apcheck (arr)
              (thisproc <? arr 0 stop))]
    [(<? arr start stop)
     (apcheck (arr)
              (pcheck ([procedure? <?] [natural? start stop])
                      (let ([len (asize arr)])
                        (when (fx> stop len)
                          (errorf who "stop index ~a out of bound ~a" stop len))
                        (when (fx> start stop)
                          (errorf who "start index ~a greater than stop index ~a" start stop))
                        (let* ([vsort! (cond [(fxarray? arr) fxvsort!]
                                             [(flarray? arr) flvsort!]
                                             [else vsort!])]
                               [acopy! (cond [(fxarray? arr) fxarray-copy!]
                                             [(flarray? arr) flarray-copy!]
                                             [else array-copy!])]
                               [newarr (amake (fx- stop start))])
                          (acopy! arr start newarr 0 (fx- stop start))
                          (let ([vec (array-vec newarr)])
                            (vsort! <? vec)
                            newarr)))))])



  #|doc
  The `*array-sort!` procedures use the binary comparison procedure `<?` to sort the array `arr`, in place.
  If only two arguments are given, the entire array is sorted;
  If the `stop` argument is given, the range from 0 to `stop-1` in `arr` is sorted;
  If both `start` and `stop` are given, the range from `start` to `stop-1` in `arr` is sorted.

  `start` and `stop` must satisfy the requirement that `0 <= start <= stop <= length of arr`.
  |#
  (define-array-procedure (a fxa fla) sort!
    [(<? arr)
     (apcheck (arr)
              (thisproc <? arr 0 (asize arr)))]
    [(<? arr stop)
     (apcheck (arr)
              (thisproc <? arr 0 stop))]
    [(<? arr start stop)
     (apcheck (arr)
              (pcheck ([procedure? <?] [natural? start stop])
                      (let ([len (asize arr)])
                        (when (fx> stop len)
                          (errorf who "stop index ~a out of bound ~a" stop len))
                        (when (fx> start stop)
                          (errorf who "start index ~a greater than stop index ~a" start stop))
                        (let ([vsort! (cond [(fxarray? arr) fxvsort!]
                                            [(flarray? arr) flvsort!]
                                            [else vsort!])]
                              [vec (array-vec arr)])
                          (vsort! <? vec start stop)))))])


  #|doc
  `n` must be a natural number.
  This procedure creates an array that contains numbers ranging from 0 to n-1, inclusive.
  This is similar to `iota` for lists.

  Note that for bytearrays, it is an error if `n` exceeds 257.
  |#
  (define-array-procedure (a fxa fla)
    (iota n)
    (pcheck ([natural? n])
            (let ([v (vmake n)])
              (let loop ([i 0])
                (if (fx= i n)
                    (amk v 2 n)
                    (begin (vset! v i (if (flvector? v) (inexact i) i))
                           (loop (fx1+ i))))))))


  #|doc
  Generate an array of of numbers: start, start+step*1, start+step*2, ...

  `start`, `stop` and `step` must be numbers that meet the following requirements:
  If `start` is less than `stop`, then `step` must be greater than 0,
  in which case the sequence terminates when the value is greater than or equal to `stop`;
  If `start` is greater than `stop`, then `step` must be less than 0,
  in which case the sequence terminates when the value is less than or equal to `stop`.

  Note that for bytearrays, it is an error if the numbers contain values that are negative or greater than 256.
  For fxarrays, the generated numbers must be fixnums.
  |#
  (define-array-procedure (a) nums
    [(stop) (thisproc 0 stop 1)]
    [(start stop) (thisproc start stop 1)]
    [(start stop step)
     (pcheck ([number? start stop step])
             (if (or (and (<= start stop) (> step 0))
                     (and (>= start stop) (< step 0)))
                 (let* ([len (exact (ceiling (/ (- stop start) step)))]
                        [vec (vmake len 0)])
                   (let loop ([i 0] [x start])
                     (if (fx= i len)
                         (amk vec 2 len)
                         (begin (vset! vec i x)
                                (loop (fx1+ i) (+ x step))))))
                 (errorf who "invalid range: ~a, ~a, ~a" start stop step)))])

  (define-array-procedure (fxa) nums
    [(stop) (thisproc 0 stop 1)]
    [(start stop) (thisproc start stop 1)]
    [(start stop step)
     (pcheck ([integer? start stop step])
             (if (or (and (<= start stop) (> step 0))
                     (and (>= start stop) (< step 0)))
                 (let* ([len (ceiling (/ (fx- stop start) step))]
                        [vec (vmake len 0)])
                   (let loop ([i 0] [x start])
                     (if (fx= i len)
                         (amk vec 2 len)
                         (begin (vset! vec i x)
                                (loop (fx1+ i) (+ x step))))))
                 (errorf who "invalid range: ~a, ~a, ~a" start stop step)))])

  #|proc:flarray-nums
  Return a flarray containing the progression from `start` toward `stop` by `step`.
  |#
  (define-who flarray-nums
    (case-lambda
      [(stop) (flarray-nums 0.0 stop 1.0)]
      [(start stop) (flarray-nums start stop 1.0)]
      [(start stop step)
       (pcheck ([number? start stop step])
               (let ([start (inexact start)] [stop (inexact stop)] [step (inexact step)])
                 (unless (or (and (fl<= start stop) (fl> step 0.0))
                             (and (fl>= start stop) (fl< step 0.0)))
                   (errorf who "invalid range: ~a, ~a, ~a" start stop step))
                 (let* ([len (exact (ceiling (fl/ (fl- stop start) step)))]
                        [arr (make-flarray len)])
                   (let loop ([i 0] [value start])
                     (if (fx= i len)
                         arr
                         (begin
                           (flarray-set! arr i value)
                           (loop (fx1+ i) (fl+ value step))))))))]))


  #|doc
  Add the item `v` to the front of the array `arr`.
  |#
  (define-array-procedure (a fxa fla)
    (push! arr v)
    (apcheck (arr)
             (aval? v)
             (let* ([len (asize arr)] [vec (array-vec arr)] [cap (vlength vec)])
               (when (fx= len cap) ($grow-array! arr))
               (vcopy! (array-vec arr) 0 (array-vec arr) 1 len)
               (vset! (array-vec arr) 0 v)
               ($array-size-set! arr (fx1+ len)))))


  #|doc
  Remove the first item from the array `arr` and return it.
  It is an error if the array is empty.
  |#
  (define-array-procedure (a fxa fla)
    (pop! arr)
    (apcheck (arr)
             (let ([len (asize arr)])
               (if (fx= len 0)
                   (errorf who "array is empty")
                   (let* ([len (asize arr)] [vec (array-vec arr)]
                          [v (vref vec 0)])
                     (vcopy! vec 1 vec 0 (fx1- len))
                     ($array-size-set! arr (fx1- len))
                     v)))))


  #|doc
  Add the item `v` to the back of the array `arr`.
  |#
  (define-array-procedure (a fxa fla)
    (push-back! arr v)
    (apcheck (arr)
             (aval? v)
             (let* ([len (asize arr)] [vec (array-vec arr)] [cap (vlength vec)])
               (when (fx= len cap) ($grow-array! arr))
               (vset! (array-vec arr) len v)
               ($array-size-set! arr (fx1+ len)))))


  #|doc
  Remove the last item from the array `arr` and return it.
  It is an error if the array is empty.
  |#
  (define-array-procedure (a fxa fla)
    (pop-back! arr)
    (apcheck (arr)
             (let ([len (asize arr)])
               (if (fx= len 0)
                   (errorf who "array is empty")
                   (let* ([len (asize arr)] [vec (array-vec arr)]
                          [v (vref vec (fx1- len))])
                     ($array-size-set! arr (fx1- len))
                     v)))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   iterations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (define check-length
    (case-lambda
      [(who arr0 arr1)
       (unless (fx= ($array-size arr0) ($array-size arr1))
         (errorf who "arrays are not of the same length"))]
      [(who arr0 . arr*)
       (unless (null? arr*)
         (unless (apply fx= ($array-size arr0) (map $array-size arr*))
           (errorf who "arrays are not of the same length")))]))


  (define-array-procedure (a fxa fla) map
    [(proc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)]
                             [newarr (amake len0)]      [newvec (array-vec newarr)])
                        (let loop ([i 0])
                          (if (fx= i len0)
                              newarr
                              (begin (vset! newvec i (proc (vref vec0 i)))
                                     (loop (fx1+ i))))))))]
    [(proc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length who arr0 arr1)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)]
                             [newarr (amake len0)]      [newvec (array-vec newarr)])
                        (let loop ([i 0])
                          (if (fx= i len0)
                              newarr
                              (begin (vset! newvec i (proc (vref vec0 i) (vref vec1 i)))
                                     (loop (fx1+ i))))))))]
    [(proc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length who arr0 arr*)
             (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)]
                    [newarr (amake len0)]      [newvec (array-vec newarr)])
               (let loop ([i 0])
                 (if (fx= i len0)
                     newarr
                     (begin (vset! newvec i (apply proc (vref vec0 i) (map (lambda (x) (vref x i)) vec*)))
                            (loop (fx1+ i)))))))])


  (define-array-procedure (a fxa fla) map/i
    [(proc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)]
                             [newarr (amake len0)]      [newvec (array-vec newarr)])
                        (let loop ([i 0])
                          (if (fx= i len0)
                              newarr
                              (begin (vset! newvec i (proc i (vref vec0 i)))
                                     (loop (fx1+ i))))))))]
    [(proc arr0 arr1)
     (pcheck ([procedure? proc])
             (check-length who arr0 arr1)
             (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)]
                    [newarr (amake len0)]      [newvec (array-vec newarr)])
               (let loop ([i 0])
                 (if (fx= i len0)
                     newarr
                     (begin (vset! newvec i (proc i (vref vec0 i) (vref vec1 i)))
                            (loop (fx1+ i)))))))]
    [(proc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length who arr0 arr*)
             (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)]
                    [newarr (amake len0)]      [newvec (array-vec newarr)])
               (let loop ([i 0])
                 (if (fx= i len0)
                     newarr
                     (begin (vset! newvec i (apply proc i (vref vec0 i) (map (lambda (x) (vref x i)) vec*)))
                            (loop (fx1+ i)))))))])


;;;; in-place maps

  (define-array-procedure (a fxa fla) map!
    [(proc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)])
                        (let loop ([i 0])
                          (if (fx= i len0)
                              arr0
                              (begin (vset! vec0 i (proc (vref vec0 i)))
                                     (loop (fx1+ i))))))))]
    [(proc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length who arr0 arr1)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)])
                        (let loop ([i 0])
                          (if (fx= i len0)
                              arr0
                              (begin (vset! vec0 i (proc (vref vec0 i) (vref vec1 i)))
                                     (loop (fx1+ i))))))))]
    [(proc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length who arr0 arr*)
             (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)])
               (let loop ([i 0])
                 (if (fx= i len0)
                     arr0
                     (begin (vset! vec0 i (apply proc (vref vec0 i) (map (lambda (x) (vref x i)) vec*)))
                            (loop (fx1+ i)))))))])


  (define-array-procedure (a fxa fla) map/i!
    [(proc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)])
                        (let loop ([i 0])
                          (if (fx= i len0)
                              arr0
                              (begin (vset! vec0 i (proc i (vref vec0 i)))
                                     (loop (fx1+ i))))))))]
    [(proc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length who arr0 arr1)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)])
                        (let loop ([i 0])
                          (if (fx= i len0)
                              arr0
                              (begin (vset! vec0 i (proc i (vref vec0 i) (vref vec1 i)))
                                     (loop (fx1+ i))))))))]
    [(proc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length who arr0 arr*)
             (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)])
               (let loop ([i 0])
                 (if (fx= i len0)
                     arr0
                     (begin (vset! vec0 i (apply proc i (vref vec0 i) (map (lambda (x) (vref x i)) vec*)))
                            (loop (fx1+ i)))))))])


  (define-array-procedure (a fxa fla) for-each
    [(proc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)])
                        (let loop ([i 0])
                          (unless (fx= i len0)
                            (proc (vref vec0 i))
                            (loop (fx1+ i)))))))]
    [(proc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length who arr0 arr1)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)])
                        (let loop ([i 0])
                          (unless (fx= i len0)
                            (proc (vref vec0 i) (vref vec1 i))
                            (loop (fx1+ i)))))))]
    [(proc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length who arr0 arr*)
             (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)])
               (let loop ([i 0])
                 (unless (fx= i len0)
                   (apply proc (vref vec0 i) (map (lambda (x) (vref x i)) vec*))
                   (loop (fx1+ i))))))])


  (define-array-procedure (a fxa fla) for-each/i
    [(proc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)])
                        (let loop ([i 0])
                          (unless (fx= i len0)
                            (proc i (vref vec0 i))
                            (loop (fx1+ i)))))))]
    [(proc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length who arr0 arr1)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)])
                        (let loop ([i 0])
                          (unless (fx= i len0)
                            (proc i (vref vec0 i) (vref vec1 i))
                            (loop (fx1+ i)))))))]
    [(proc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length who arr0 arr*)
             (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)])
               (let loop ([i 0])
                 (unless (fx= i len0)
                   (apply proc i (vref vec0 i) (map (lambda (x) (vref x i)) vec*))
                   (loop (fx1+ i))))))])


;;;; reverse order

  (define-array-procedure (a fxa fla) map-rev
    [(proc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)]
                             [newarr (amake len0)]      [newvec (array-vec newarr)])
                        (let loop ([i (fx1- len0)] [j 0])
                          (if (fx= i -1)
                              newarr
                              (begin (vset! newvec j (proc (vref vec0 i)))
                                     (loop (fx1- i) (fx1+ j))))))))]
    [(proc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length who arr0 arr1)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)]
                             [newarr (amake len0)]      [newvec (array-vec newarr)])
                        (let loop ([i (fx1- len0)] [j 0])
                          (if (fx= i -1)
                              newarr
                              (begin (vset! newvec j (proc (vref vec0 i) (vref vec1 i)))
                                     (loop (fx1- i) (fx1+ j))))))))]
    [(proc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length who arr0 arr*)
             (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)]
                    [newarr (amake len0)]      [newvec (array-vec newarr)])
               (let loop ([i (fx1- len0)] [j 0])
                 (if (fx= i -1)
                     newarr
                     (begin (vset! newvec j (apply proc (vref vec0 i) (map (lambda (x) (vref x i)) vec*)))
                            (loop (fx1- i) (fx1+ j)))))))])


  (define-array-procedure (a fxa fla) map/i-rev
    [(proc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)]
                             [newarr (amake len0)]      [newvec (array-vec newarr)])
                        (let loop ([i (fx1- len0)] [j 0])
                          (if (fx= i -1)
                              newarr
                              (begin (vset! newvec j (proc i (vref vec0 i)))
                                     (loop (fx1- i) (fx1+ j))))))))]
    [(proc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length who arr0 arr1)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)]
                             [newarr (amake len0)]      [newvec (array-vec newarr)])
                        (let loop ([i (fx1- len0)] [j 0])
                          (if (fx= i -1)
                              newarr
                              (begin (vset! newvec j (proc i (vref vec0 i) (vref vec1 i)))
                                     (loop (fx1- i) (fx1+ j))))))))]
    [(proc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length who arr0 arr*)
             (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)]
                    [newarr (amake len0)]      [newvec (array-vec newarr)])
               (let loop ([i (fx1- len0)] [j 0])
                 (if (fx= i -1)
                     newarr
                     (begin (vset! newvec j (apply proc i (vref vec0 i) (map (lambda (x) (vref x i)) vec*)))
                            (loop (fx1- i) (fx1+ j)))))))])


  (define-array-procedure (a fxa fla) for-each-rev
    [(proc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)])
                        (let loop ([i (fx1- len0)])
                          (unless (fx= i -1)
                            (proc (vref vec0 i))
                            (loop (fx1- i)))))))]
    [(proc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length who arr0 arr1)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)])
                        (let loop ([i (fx1- len0)])
                          (unless (fx= i -1)
                            (proc (vref vec0 i) (vref vec1 i))
                            (loop (fx1- i)))))))]
    [(proc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length who arr0 arr*)
             (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)])
               (let loop ([i (fx1- len0)] [j 0])
                 (unless (fx= i -1)
                   (apply proc (vref vec0 i) (map (lambda (x) (vref x i)) vec*))
                   (loop (fx1- i) (fx1+ j))))))])


  (define-array-procedure (a fxa fla) for-each/i-rev
    [(proc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)])
                        (let loop ([i (fx1- len0)])
                          (unless (fx= i -1)
                            (proc i (vref vec0 i))
                            (loop (fx1- i)))))))]
    [(proc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length who arr0 arr1)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)])
                        (let loop ([i (fx1- len0)])
                          (unless (fx= i -1)
                            (proc i (vref vec0 i) (vref vec1 i))
                            (loop (fx1- i)))))))]
    [(proc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length who arr0 arr*)
             (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)])
               (let loop ([i (fx1- len0)] [j 0])
                 (unless (fx= i -1)
                   (apply proc i (vref vec0 i) (map (lambda (x) (vref x i)) vec*))
                   (loop (fx1- i) (fx1+ j))))))])


  (define-array-procedure (a fxa fla) andmap
    [(proc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)])
                        (let loop ([i 0])
                          (if (fx= i len0)
                              #t
                              (and (proc (vref vec0 i))
                                   (loop (fx1+ i))))))))]
    [(proc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length who arr0 arr1)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)])
                        (let loop ([i 0])
                          (if (fx= i len0)
                              #t
                              (and (proc (vref vec0 i) (vref vec1 i))
                                   (loop (fx1+ i))))))))]
    [(proc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length who arr0 arr*)
             (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)])
               (let loop ([i 0])
                 (if (fx= i len0)
                     #t
                     (and (apply proc (vref vec0 i) (map (lambda (x) (vref x i)) vec*))
                          (loop (fx1+ i)))))))])


  (define-array-procedure (a fxa fla) ormap
    [(proc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)])
                        (let loop ([i 0])
                          (if (fx= i len0)
                              #f
                              (or (proc (vref vec0 i))
                                  (loop (fx1+ i))))))))]
    [(proc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length who arr0 arr1)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)])
                        (let loop ([i 0])
                          (if (fx= i len0)
                              #f
                              (or (proc (vref vec0 i) (vref vec1 i))
                                  (loop (fx1+ i))))))))]
    [(proc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length who arr0 arr*)
             (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)])
               (let loop ([i 0])
                 (if (fx= i len0)
                     #f
                     (or (apply proc (vref vec0 i) (map (lambda (x) (vref x i)) vec*))
                         (loop (fx1+ i)))))))])


;;;; folds


  (define-array-procedure (a fxa fla) fold-left
    [(proc acc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)])
                        (let loop ([acc acc] [i 0])
                          (if (fx= i len0)
                              acc
                              (loop (proc acc (vref vec0 i))
                                    (fx1+ i)))))))]
    [(proc acc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length who arr0 arr1)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)])
                        (let loop ([acc acc] [i 0])
                          (if (fx= i len0)
                              acc
                              (loop (proc acc (vref vec0 i) (vref vec1 i))
                                    (fx1+ i)))))))]
    [(proc acc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length who arr0 arr*)
             (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)])
               (let loop ([acc acc] [i 0])
                 (if (fx= i len0)
                     acc
                     (loop (apply proc acc (vref vec0 i) (map (lambda (x) (vref x i)) vec*))
                           (fx1+ i))))))])


  (define-array-procedure (a fxa fla) fold-left/i
    [(proc acc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)])
                        (let loop ([acc acc] [i 0])
                          (if (fx= i len0)
                              acc
                              (loop (proc i acc (vref vec0 i))
                                    (fx1+ i)))))))]
    [(proc acc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length who arr0 arr1)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)])
                        (let loop ([acc acc] [i 0])
                          (if (fx= i len0)
                              acc
                              (loop (proc i acc (vref vec0 i) (vref vec1 i))
                                    (fx1+ i)))))))]
    [(proc acc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length who arr0 arr*)
             (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)])
               (let loop ([acc acc] [i 0])
                 (if (fx= i len0)
                     acc
                     (loop (apply proc i acc (vref vec0 i) (map (lambda (x) (vref x i)) vec*))
                           (fx1+ i))))))])


  (define-array-procedure (a fxa fla) fold-right
    [(proc acc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)])
                        (let loop ([acc acc] [i (fx1- len0)])
                          (if (fx= i -1)
                              acc
                              (loop (proc (vref vec0 i) acc)
                                    (fx1- i)))))))]
    [(proc acc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)])
                        (let loop ([acc acc] [i (fx1- len0)])
                          (if (fx= i -1)
                              acc
                              (loop (proc (vref vec0 i) (vref vec1 i) acc)
                                    (fx1- i)))))))]
    [(proc acc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length who arr0 arr*)
             (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)])
               (let loop ([acc acc] [i (fx1- len0)])
                 (if (fx= i -1)
                     acc
                     (loop (apply proc (vref vec0 i) `(,@(map (lambda (x) (vref x i)) vec*) ,acc))
                           (fx1- i))))))])


  (define-array-procedure (a fxa fla) fold-right/i
    [(proc acc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)])
                        (let loop ([acc acc] [i (fx1- len0)])
                          (if (fx= i -1)
                              acc
                              (loop (proc i (vref vec0 i) acc)
                                    (fx1- i)))))))]
    [(proc acc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)])
                        (let loop ([acc acc] [i (fx1- len0)])
                          (if (fx= i -1)
                              acc
                              (loop (proc i (vref vec0 i) (vref vec1 i) acc)
                                    (fx1- i)))))))]
    [(proc acc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length who arr0 arr*)
             (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)])
               (let loop ([acc acc] [i (fx1- len0)])
                 (if (fx= i -1)
                     acc
                     (loop (apply proc i (vref vec0 i) `(,@(map (lambda (x) (vref x i)) vec*) ,acc))
                           (fx1- i))))))])



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   conversions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  #|doc
  Convert a list to an array.
  |#
  (define-who list->array
    (lambda (ls)
      (pcheck ([list? ls])
              (apply array ls))))


  #|proc:vector->array
  Convert a vector `vec` to an array.
  |#
  (define-who vector->array
    (lambda (vec)
      (pcheck ([vector? vec])
              (let* ([len (vector-length vec)]
                     [arr (make-array len)])
                (let loop ([i 0])
                  (if (fx= i len)
                      arr
                      (begin (array-set! arr i (vector-ref vec i))
                             (loop (fx1+ i)))))))))

  #|proc:fxvector->fxarray
  Convert a fxvector `vec` to a fxarray.
  |#
  (define-who fxvector->fxarray
    (lambda (vec)
      (pcheck ([fxvector? vec])
              (let* ([len (fxvector-length vec)]
                     [arr (make-fxarray len)])
                (let loop ([i 0])
                  (if (fx= i len)
                      arr
                      (begin (fxarray-set! arr i (fxvector-ref vec i))
                             (loop (fx1+ i)))))))))

  #|proc:bytevector->bytearray
  Convert a bytevector/u8vector `vec` to a bytearray.
  |#
  (define-who bytevector->bytearray
    (lambda (vec)
      (pcheck ([bytevector? vec])
              (let* ([len (bytevector-length vec)]
                     [arr (make-bytearray len)])
                (let loop ([i 0])
                  (if (fx= i len)
                      arr
                      (begin (bytearray-set! arr i (bytevector-u8-ref vec i))
                             (loop (fx1+ i)))))))))


  #|
  Convert an array to a list.
  |#
  ;; defines {,fx,u8}array->list
  (define-array-procedure (a fxa fla)
    (>list arr)
    (apcheck (arr)
             (let ([lb (make-list-builder)] [vec (array-vec arr)] [len ($array-size arr)])
               (let loop ([i 0])
                 (if (fx= i len)
                     (lb)
                     (begin (lb (vref vec i))
                            (loop (fx1+ i))))))))


  #|proc:array->iter
  The `array->iter` procedure returns an iterator over values in the array `source`.
  `(source)` traverses the current full array and reevaluates its size on reset.
  `(source stop)` defaults `start` to 0, and `(source start stop)` defaults `step` to 1.
  `(source start stop step)` selects a half-open indexed range with a nonzero integer `step`.
  A positive `step` visits increasing indexes at that stride; a negative `step` visits
  decreasing indexes at the absolute stride. The iterator returns each selected value.
  |#
  (define array->iter
    (make-indexed-iter 'array->iter array? array-size array-ref))
  #|proc:flarray->iter
  Return an iterator over the current values of flarray `source`.
  |#
  (define flarray->iter
    (make-indexed-iter 'flarray->iter flarray? flarray-size flarray-ref))

  #|proc:fxarray->iter
  The `fxarray->iter` procedure returns an iterator over fixnums in the fxarray `source`.
  `(source)` traverses the current full fxarray and reevaluates its size on reset.
  `(source stop)` defaults `start` to 0, and `(source start stop)` defaults `step` to 1.
  `(source start stop step)` selects a half-open indexed range with a nonzero integer `step`.
  A positive `step` visits increasing indexes at that stride; a negative `step` visits
  decreasing indexes at the absolute stride. The iterator returns each selected fixnum.
  |#
  (define fxarray->iter
    (make-indexed-iter 'fxarray->iter fxarray? fxarray-size fxarray-ref))

  #|proc:bytearray->iter
  The `bytearray->iter` procedure returns an iterator over bytes in the bytearray `source`.
  `(source)` traverses the current full bytearray and reevaluates its size on reset.
  `(source stop)` defaults `start` to 0, and `(source start stop)` defaults `step` to 1.
  `(source start stop step)` selects a half-open indexed range with a nonzero integer `step`.
  A positive `step` visits increasing indexes at that stride; a negative `step` visits
  decreasing indexes at the absolute stride. The iterator returns each selected byte.
  |#
  (define bytearray->iter
    (make-indexed-iter 'bytearray->iter bytearray?
                       (lambda (arr) ($array-size arr))
                       (lambda (arr index)
                         (bytevector-u8-ref (array-vec arr) index))))


  #|proc:array->vector
  Convert an array `arr` into a vector.
  |#
  (define-who array->vector
    (lambda (arr)
      (pcheck ([array? arr])
              (let* ([len ($array-size arr)]
                     [vec (make-vector len)])
                (let loop ([i 0])
                  (if (fx= i len)
                      vec
                      (begin (vector-set! vec i (array-ref arr i))
                             (loop (fx1+ i)))))))))


  #|proc:fxarray->fxvector
  Convert a fxarray `arr` into a fxvector.
  |#
  (define-who fxarray->fxvector
    (lambda (arr)
      (pcheck ([fxarray? arr])
              (let* ([len (fxarray-size arr)]
                     [vec (make-fxvector len)])
                (let loop ([i 0])
                  (if (fx= i len)
                      vec
                      (begin (fxvector-set! vec i (fxarray-ref arr i))
                             (loop (fx1+ i)))))))))

  #|proc:bytearray->bytevector
  Convert a bytearray `arr` into a bytevector.
  |#
  (define-who bytearray->bytevector
    (lambda (arr)
      (pcheck ([bytearray? arr])
              (let* ([len (bytearray-size arr)]
                     [vec (make-bytevector len)])
                (let loop ([i 0])
                  (if (fx= i len)
                      vec
                      (begin (bytevector-u8-set! vec i (bytearray-ref arr i))
                             (loop (fx1+ i)))))))))

  (define-syntax define-bytearray-accessors
    (syntax-rules ()
      [(_ ref-name set-name width-ref width-set width)
       (define-bytearray-accessors
         ref-name set-name width-ref width-set width (lambda (value) #t))]
      [(_ ref-name set-name width-ref width-set width pred)
       (begin
         (define-who ref-name
           (lambda (arr i)
             (pcheck ([bytearray? arr] [natural? i])
                     (let ([n (bytearray-size arr)])
                       (when (not (fx= (modulo n width) 0))
                         (errorf who "bytearray length is not aligned to width ~a" width))
                       (if (fx< i (fx/ n width))
                           (width-ref (array-vec arr) (fx* i width))
                           (errorf who "index ~a out of range" i))))))
         (define-who set-name
           (lambda (arr i v)
             (pcheck ([bytearray? arr] [natural? i])
                     (let ([n (bytearray-size arr)])
                       (when (not (fx= (modulo n width) 0))
                         (errorf who "bytearray length is not aligned to width ~a" width))
                       (if (fx< i (fx/ n width))
                           (begin (unless (pred v) (errorf who "value out of range for width ~a: ~a" width v))
                                  (width-set (array-vec arr) (fx* i width) v))
                                  (errorf who "index ~a out of range" i)))))))]))
  (define bytearray-u8-value? (lambda (v) (and (and (integer? v) (exact? v)) (<= 0 v 255))))
  (define bytearray-s8-value? (lambda (v) (and (and (integer? v) (exact? v)) (<= -128 v 127))))
  (define bytearray-u16-value? (lambda (v) (and (and (integer? v) (exact? v)) (<= 0 v 65535))))
  (define bytearray-s16-value? (lambda (v) (and (and (integer? v) (exact? v)) (<= -32768 v 32767))))
  (define bytearray-u24-value? (lambda (v) (and (and (integer? v) (exact? v)) (<= 0 v 16777215))))
  (define bytearray-s24-value? (lambda (v) (and (and (integer? v) (exact? v)) (<= -8388608 v 8388607))))
  (define bytearray-u32-value? (lambda (v) (and (and (integer? v) (exact? v)) (<= 0 v 4294967295))))
  (define bytearray-s32-value? (lambda (v) (and (and (integer? v) (exact? v)) (<= -2147483648 v 2147483647))))
  (define bytearray-u40-value? (lambda (v) (and (and (integer? v) (exact? v)) (<= 0 v 1099511627775))))
  (define bytearray-s40-value? (lambda (v) (and (and (integer? v) (exact? v)) (<= -549755813888 v 549755813887))))
  (define bytearray-u48-value? (lambda (v) (and (and (integer? v) (exact? v)) (<= 0 v 281474976710655))))
  (define bytearray-s48-value? (lambda (v) (and (and (integer? v) (exact? v)) (<= -140737488355328 v 140737488355327))))
  (define bytearray-u56-value? (lambda (v) (and (and (integer? v) (exact? v)) (<= 0 v 72057594037927935))))
  (define bytearray-s56-value? (lambda (v) (and (and (integer? v) (exact? v)) (<= -36028797018963968 v 36028797018963967))))
  (define bytearray-u64-value? (lambda (v) (and (and (integer? v) (exact? v)) (<= 0 v 18446744073709551615))))
  (define bytearray-s64-value? (lambda (v) (and (and (integer? v) (exact? v)) (<= -9223372036854775808 v 9223372036854775807))))
  (define-bytearray-accessors bytearray-u16-ref bytearray-u16-set!
    (lambda (bv i) (bytevector-u16-ref bv i (endianness little)))
    (lambda (bv i v) (bytevector-u16-set! bv i v (endianness little))) 2
    (lambda (v) (and (and (integer? v) (exact? v)) (<= 0 v 65535))))
  (define-bytearray-accessors bytearray-U16-ref bytearray-U16-set!
    (lambda (bv i) (bytevector-u16-ref bv i (endianness big)))
    (lambda (bv i v) (bytevector-u16-set! bv i v (endianness big))) 2
    (lambda (v) (and (and (integer? v) (exact? v)) (<= 0 v 65535))))
  (define-bytearray-accessors bytearray-s16-ref bytearray-s16-set!
    (lambda (bv i) (bytevector-s16-ref bv i (endianness little)))
    (lambda (bv i v) (bytevector-s16-set! bv i v (endianness little))) 2)
  (define-bytearray-accessors bytearray-S16-ref bytearray-S16-set!
    (lambda (bv i) (bytevector-s16-ref bv i (endianness big)))
    (lambda (bv i v) (bytevector-s16-set! bv i v (endianness big))) 2)
  (define-bytearray-accessors bytearray-fp32-ref bytearray-fp32-set!
    (lambda (bv i) (bytevector-ieee-single-ref bv i (endianness little)))
    (lambda (bv i v) (bytevector-ieee-single-set! bv i v (endianness little))) 4)
  (define-bytearray-accessors bytearray-FP32-ref bytearray-FP32-set!
    (lambda (bv i) (bytevector-ieee-single-ref bv i (endianness big)))
    (lambda (bv i v) (bytevector-ieee-single-set! bv i v (endianness big))) 4)
  (define-bytearray-accessors bytearray-u8-ref bytearray-u8-set!
    (lambda (bv i) (bytevector-u8-ref bv i))
    (lambda (bv i v) (bytevector-u8-set! bv i v)) 1 bytearray-u8-value?)
  (define-bytearray-accessors bytearray-U8-ref bytearray-U8-set!
    (lambda (bv i) (bytevector-u8-ref bv i))
    (lambda (bv i v) (bytevector-u8-set! bv i v)) 1 bytearray-u8-value?)
  (define-bytearray-accessors bytearray-s8-ref bytearray-s8-set!
    (lambda (bv i) (bytevector-s8-ref bv i))
    (lambda (bv i v) (bytevector-s8-set! bv i v)) 1 bytearray-s8-value?)
  (define-bytearray-accessors bytearray-S8-ref bytearray-S8-set!
    (lambda (bv i) (bytevector-s8-ref bv i))
    (lambda (bv i v) (bytevector-s8-set! bv i v)) 1 bytearray-s8-value?)
  (define-bytearray-accessors bytearray-u24-ref bytearray-u24-set! (lambda (bv i) (bytevector-u24-ref bv i (endianness little))) (lambda (bv i v) (bytevector-u24-set! bv i v (endianness little))) 3 bytearray-u24-value?)
  (define-bytearray-accessors bytearray-U24-ref bytearray-U24-set! (lambda (bv i) (bytevector-u24-ref bv i (endianness big))) (lambda (bv i v) (bytevector-u24-set! bv i v (endianness big))) 3 bytearray-u24-value?)
  (define-bytearray-accessors bytearray-s24-ref bytearray-s24-set! (lambda (bv i) (bytevector-s24-ref bv i (endianness little))) (lambda (bv i v) (bytevector-s24-set! bv i v (endianness little))) 3 bytearray-s24-value?)
  (define-bytearray-accessors bytearray-S24-ref bytearray-S24-set! (lambda (bv i) (bytevector-s24-ref bv i (endianness big))) (lambda (bv i v) (bytevector-s24-set! bv i v (endianness big))) 3 bytearray-s24-value?)
  (define-bytearray-accessors bytearray-u32-ref bytearray-u32-set! (lambda (bv i) (bytevector-u32-ref bv i (endianness little))) (lambda (bv i v) (bytevector-u32-set! bv i v (endianness little))) 4 bytearray-u32-value?)
  (define-bytearray-accessors bytearray-U32-ref bytearray-U32-set! (lambda (bv i) (bytevector-u32-ref bv i (endianness big))) (lambda (bv i v) (bytevector-u32-set! bv i v (endianness big))) 4 bytearray-u32-value?)
  (define-bytearray-accessors bytearray-s32-ref bytearray-s32-set! (lambda (bv i) (bytevector-s32-ref bv i (endianness little))) (lambda (bv i v) (bytevector-s32-set! bv i v (endianness little))) 4 bytearray-s32-value?)
  (define-bytearray-accessors bytearray-S32-ref bytearray-S32-set! (lambda (bv i) (bytevector-s32-ref bv i (endianness big))) (lambda (bv i v) (bytevector-s32-set! bv i v (endianness big))) 4 bytearray-s32-value?)
  (define-bytearray-accessors bytearray-u40-ref bytearray-u40-set! (lambda (bv i) (bytevector-u40-ref bv i (endianness little))) (lambda (bv i v) (bytevector-u40-set! bv i v (endianness little))) 5 bytearray-u40-value?)
  (define-bytearray-accessors bytearray-U40-ref bytearray-U40-set! (lambda (bv i) (bytevector-u40-ref bv i (endianness big))) (lambda (bv i v) (bytevector-u40-set! bv i v (endianness big))) 5 bytearray-u40-value?)
  (define-bytearray-accessors bytearray-s40-ref bytearray-s40-set! (lambda (bv i) (bytevector-s40-ref bv i (endianness little))) (lambda (bv i v) (bytevector-s40-set! bv i v (endianness little))) 5 bytearray-s40-value?)
  (define-bytearray-accessors bytearray-S40-ref bytearray-S40-set! (lambda (bv i) (bytevector-s40-ref bv i (endianness big))) (lambda (bv i v) (bytevector-s40-set! bv i v (endianness big))) 5 bytearray-s40-value?)
  (define-bytearray-accessors bytearray-u48-ref bytearray-u48-set! (lambda (bv i) (bytevector-u48-ref bv i (endianness little))) (lambda (bv i v) (bytevector-u48-set! bv i v (endianness little))) 6 bytearray-u48-value?)
  (define-bytearray-accessors bytearray-U48-ref bytearray-U48-set! (lambda (bv i) (bytevector-u48-ref bv i (endianness big))) (lambda (bv i v) (bytevector-u48-set! bv i v (endianness big))) 6 bytearray-u48-value?)
  (define-bytearray-accessors bytearray-s48-ref bytearray-s48-set! (lambda (bv i) (bytevector-s48-ref bv i (endianness little))) (lambda (bv i v) (bytevector-s48-set! bv i v (endianness little))) 6 bytearray-s48-value?)
  (define-bytearray-accessors bytearray-S48-ref bytearray-S48-set! (lambda (bv i) (bytevector-s48-ref bv i (endianness big))) (lambda (bv i v) (bytevector-s48-set! bv i v (endianness big))) 6 bytearray-s48-value?)
  (define-bytearray-accessors bytearray-u56-ref bytearray-u56-set! (lambda (bv i) (bytevector-u56-ref bv i (endianness little))) (lambda (bv i v) (bytevector-u56-set! bv i v (endianness little))) 7 bytearray-u56-value?)
  (define-bytearray-accessors bytearray-U56-ref bytearray-U56-set! (lambda (bv i) (bytevector-u56-ref bv i (endianness big))) (lambda (bv i v) (bytevector-u56-set! bv i v (endianness big))) 7 bytearray-u56-value?)
  (define-bytearray-accessors bytearray-s56-ref bytearray-s56-set! (lambda (bv i) (bytevector-s56-ref bv i (endianness little))) (lambda (bv i v) (bytevector-s56-set! bv i v (endianness little))) 7 bytearray-s56-value?)
  (define-bytearray-accessors bytearray-S56-ref bytearray-S56-set! (lambda (bv i) (bytevector-s56-ref bv i (endianness big))) (lambda (bv i v) (bytevector-s56-set! bv i v (endianness big))) 7 bytearray-s56-value?)
  (define-bytearray-accessors bytearray-u64-ref bytearray-u64-set! (lambda (bv i) (bytevector-u64-ref bv i (endianness little))) (lambda (bv i v) (bytevector-u64-set! bv i v (endianness little))) 8 bytearray-u64-value?)
  (define-bytearray-accessors bytearray-U64-ref bytearray-U64-set! (lambda (bv i) (bytevector-u64-ref bv i (endianness big))) (lambda (bv i v) (bytevector-u64-set! bv i v (endianness big))) 8 bytearray-u64-value?)
  (define-bytearray-accessors bytearray-s64-ref bytearray-s64-set! (lambda (bv i) (bytevector-s64-ref bv i (endianness little))) (lambda (bv i v) (bytevector-s64-set! bv i v (endianness little))) 8 bytearray-s64-value?)
  (define-bytearray-accessors bytearray-S64-ref bytearray-S64-set! (lambda (bv i) (bytevector-s64-ref bv i (endianness big))) (lambda (bv i v) (bytevector-s64-set! bv i v (endianness big))) 8 bytearray-s64-value?)
  (define-bytearray-accessors bytearray-fp64-ref bytearray-fp64-set! (lambda (bv i) (bytevector-ieee-double-ref bv i (endianness little))) (lambda (bv i v) (bytevector-ieee-double-set! bv i v (endianness little))) 8 flonum?)
  (define-bytearray-accessors bytearray-FP64-ref bytearray-FP64-set! (lambda (bv i) (bytevector-ieee-double-ref bv i (endianness big))) (lambda (bv i v) (bytevector-ieee-double-set! bv i v (endianness big))) 8 flonum?)

  (define $bytearray-width-length
    (lambda (who arr width)
      (pcheck ([bytearray? arr])
              (let ([bytes (bytearray-size arr)])
                (unless (fx= 0 (modulo bytes width))
                  (errorf who "bytearray length ~a is not aligned to width ~a" bytes width))
                (fx/ bytes width)))))

  (define $bytearray-merge-sort!
    (lambda (less? arr start stop width ref set)
      (let ([scratch (make-bytearray (fx* width (fx- stop start)) 0)])
        (define merge!
          (lambda (left middle right)
            (let loop ([i left] [j middle] [k left])
              (cond
                [(and (fx< i middle) (fx< j right))
                 (if (less? (ref arr j) (ref arr i))
                     (begin (set scratch (fx- k start) (ref arr j))
                            (loop i (fx1+ j) (fx1+ k)))
                     (begin (set scratch (fx- k start) (ref arr i))
                            (loop (fx1+ i) j (fx1+ k))))]
                [(fx< i middle)
                 (set scratch (fx- k start) (ref arr i))
                 (loop (fx1+ i) j (fx1+ k))]
                [(fx< j right)
                 (set scratch (fx- k start) (ref arr j))
                 (loop i (fx1+ j) (fx1+ k))]
                [else
                 (let copy-back ([index left])
                   (unless (fx= index right)
                     (set arr index (ref scratch (fx- index start)))
                     (copy-back (fx1+ index))))]))))
        (define sort-range!
          (lambda (left right)
            (when (fx< (fx1+ left) right)
              (let ([middle (fx/ (fx+ left right) 2)])
                (sort-range! left middle)
                (sort-range! middle right)
                (merge! left middle right)))))
        (sort-range! start stop)
        arr)))

  (define $make-bytearray-width-operations
    (lambda (who width ref set value?)
      (define zero (if (value? 0) 0 0.0))
      (define one (if (value? 1) 1 1.0))
      (define length-of
        (lambda (arr) ($bytearray-width-length who arr width)))
      (define make-result
        (lambda (length) (make-bytearray (fx* width length) 0)))
      (define check-value
        (lambda (value)
          (unless (value? value)
            (errorf who "value is invalid for the selected width: ~a" value))))
      (define copy-array
        (lambda (arr)
          (let* ([bytes (bytearray-size arr)] [result (make-bytearray bytes 0)])
            (bytevector-copy! (array-vec arr) 0 (array-vec result) 0 bytes)
            result)))
      (define replace!
        (lambda (target source)
          (let ([bytes (bytearray-size source)] [storage (make-bytevector (bytearray-size source) 0)])
            (bytevector-copy! (array-vec source) 0 storage 0 bytes)
            (array-vec-set! target storage)
            ($array-size-set! target bytes)
            target)))
      (define source-length
        (lambda (arr arrays)
          (pcheck ([bytearray? arr] [all-bytearrays? arrays])
                  (let ([length (length-of arr)])
                    (for-each
                     (lambda (source)
                       (unless (fx= length (length-of source))
                         (errorf who "arrays differ in logical length")))
                     arrays)
                    length))))
      (define values-at
        (lambda (index arrays)
          (map (lambda (source) (ref source index)) arrays)))
      (define shrink!
        (lambda (arr logical-length)
          (let* ([bytes (fx* width logical-length)]
                 [storage (make-bytevector bytes 0)])
            (bytevector-copy! (array-vec arr) 0 storage 0 bytes)
            (array-vec-set! arr storage)
            ($array-size-set! arr bytes)
            arr)))
      (define add-values!
        (lambda (arr index values)
          (let* ([len (length-of arr)]
                 [count (length values)]
                 [old-bytes (bytearray-size arr)]
                 [insert-byte (fx* index width)]
                 [count-bytes (fx* count width)]
                 [new-bytes (fx+ old-bytes count-bytes)]
                 [old (array-vec arr)]
                 [capacity (bytevector-length old)])
            (when (fx> index len)
              (errorf who "index ~a out of range ~a" index len))
            (for-each check-value values)
            (when (fx< capacity new-bytes)
              (let* ([grown (if (fx= capacity 0)
                                *mincap*
                                (fx* capacity (array-incr-factor arr)))]
                     [new-capacity (if (fx>= grown new-bytes) grown new-bytes)]
                     [new (make-bytevector new-capacity 0)])
                (bytevector-copy! old 0 new 0 old-bytes)
                (array-vec-set! arr new)))
            (let ([storage (array-vec arr)])
              (when (fx< insert-byte old-bytes)
                (bytevector-copy! storage insert-byte storage
                                  (fx+ insert-byte count-bytes)
                                  (fx- old-bytes insert-byte)))
              ($array-size-set! arr new-bytes)
              (let loop ([i 0] [rest values])
                (unless (null? rest)
                  (set arr (fx+ index i) (car rest))
                  (loop (fx1+ i) (cdr rest))))
              arr))))
      (define add!
        (case-lambda
          [(arr value)
           (pcheck ([bytearray? arr])
                   (add-values! arr (length-of arr) (list value)))]
          [(arr index value)
           (pcheck ([bytearray? arr] [natural? index])
                   (add-values! arr index (list value)))]))
      (define add*!
        (lambda (arr . arguments)
          (pcheck ([bytearray? arr])
                  (if (null? arguments)
                      arr
                      (let ([first (car arguments)] [rest (cdr arguments)]
                            [len (length-of arr)])
                        (if (and (pair? rest) (fixnum? first) (fx>= first 0)
                                 (fx<= first len))
                            (add-values! arr first rest)
                            (add-values! arr len arguments)))))))
      (define delete!
        (lambda (arr index)
          (pcheck ([bytearray? arr] [natural? index])
                  (let ([len (length-of arr)])
                    (when (fx>= index len) (errorf who "index ~a out of range ~a" index len))
                    (let* ([storage (array-vec arr)]
                           [byte-index (fx* index width)]
                           [following (fx- (bytearray-size arr) byte-index width)])
                      (when (fx> following 0)
                        (bytevector-copy! storage (fx+ byte-index width)
                                          storage byte-index following))
                      ($array-size-set! arr (fx- (bytearray-size arr) width))
                      arr)))))
      (define slice
        (case-lambda
          [(arr stop) (slice arr 0 stop 1)]
          [(arr start stop) (slice arr start stop 1)]
          [(arr start stop step)
           (pcheck ([bytearray? arr] [fixnum? start stop step])
                   (when (fx= step 0) (errorf who "step cannot be zero"))
                   (let* ([len (length-of arr)]
                          [start0 (if (fx>= start 0) start (fx+ len start))]
                          [first (cond [(fx< start0 0) 0]
                                       [(fx> start0 len) (fx1- len)]
                                       [else start0])]
                          [stop0 (if (fx>= stop 0) stop (fx+ len stop))]
                          [last (cond [(fx<= stop0 -1) -1]
                                      [(fx>= stop0 len) len]
                                      [else stop0])]
                          [count (if (fx= len 0)
                                     0
                                     (let count ([i first] [n 0])
                                       (if (if (fx> step 0) (fx>= i last) (fx<= i last))
                                           n
                                           (count (fx+ i step) (fx1+ n)))))]
                          [result (make-result count)])
                     (let fill ([i first] [output 0])
                       (if (fx= output count)
                           result
                           (begin (set result output (ref arr i))
                                  (fill (fx+ i step) (fx1+ output)))))))]))
      (define slice!
        (case-lambda
          [(arr stop) (slice! arr 0 stop 1)]
          [(arr start stop) (slice! arr start stop 1)]
          [(arr start stop step) (replace! arr (slice arr start stop step))]))
      (define copy (lambda (arr) (pcheck ([bytearray? arr]) (length-of arr) (copy-array arr))))
      (define copy!
        (lambda (src src-start target target-start count)
          (pcheck ([bytearray? src target] [natural? src-start target-start count])
                  (let ([source-length (length-of src)] [target-length (length-of target)])
                    (when (fx> (fx+ src-start count) source-length)
                      (errorf who "source range is too large"))
                    (when (fx> (fx+ target-start count) target-length)
                      (errorf who "target range is too large"))
                    (bytevector-copy! (array-vec src) (fx* src-start width)
                                      (array-vec target) (fx* target-start width)
                                      (fx* count width))
                    target))))
      (define push! (lambda (arr value) (add! arr 0 value)))
      (define pop!
        (lambda (arr)
          (let ([value (ref arr 0)]) (delete! arr 0) value)))
      (define push-back! (lambda (arr value) (add! arr value)))
      (define pop-back!
        (lambda (arr)
          (let* ([len (length-of arr)] [value (ref arr (fx1- len))])
            (delete! arr (fx1- len)) value)))
      (define filter
        (lambda (pred arr)
          (pcheck ([procedure? pred] [bytearray? arr])
                  (let* ([len (length-of arr)] [result (make-result len)])
                    (let loop ([i 0] [output 0])
                      (if (fx= i len)
                          (shrink! result output)
                          (let ([value (ref arr i)])
                            (if (pred value)
                                (begin (set result output value)
                                       (loop (fx1+ i) (fx1+ output)))
                                (loop (fx1+ i) output)))))))))
      (define filter!
        (lambda (pred arr) (replace! arr (filter pred arr))))
      (define partition
        (lambda (pred arr)
          (pcheck ([procedure? pred] [bytearray? arr])
                  (let* ([len (length-of arr)] [yes (make-result len)] [no (make-result len)])
                    (let loop ([i 0] [yes-count 0] [no-count 0])
                      (if (fx= i len)
                          (values (shrink! yes yes-count) (shrink! no no-count))
                          (let ([value (ref arr i)])
                            (if (pred value)
                                (begin (set yes yes-count value)
                                       (loop (fx1+ i) (fx1+ yes-count) no-count))
                                (begin (set no no-count value)
                                       (loop (fx1+ i) yes-count (fx1+ no-count)))))))))))
      (define contains?
        (lambda (arr value)
          (let ([len (length-of arr)])
            (let loop ([i 0])
              (and (fx< i len)
                   (or (equal? value (ref arr i)) (loop (fx1+ i))))))))
      (define contains/p?
        (lambda (arr pred)
          (pcheck ([procedure? pred])
                  (let ([len (length-of arr)])
                    (let loop ([i 0])
                      (and (fx< i len) (or (pred (ref arr i)) (loop (fx1+ i)))))))))
      (define index-of
        (lambda (arr value)
          (let ([len (length-of arr)])
            (let loop ([i 0])
              (cond [(fx= i len) #f] [(equal? value (ref arr i)) i]
                    [else (loop (fx1+ i))])))))
      (define find-index
        (lambda (arr pred)
          (pcheck ([procedure? pred])
                  (let ([len (length-of arr)])
                    (let loop ([i 0])
                      (cond [(fx= i len) #f] [(pred (ref arr i)) i]
                            [else (loop (fx1+ i))]))))))
      (define search
        (lambda (arr pred)
          (let ([index (find-index arr pred)]) (and index (ref arr index)))))
      (define search*
        (case-lambda
          [(arr pred)
           (let ([len (length-of arr)])
             (let loop ([i 0] [result '()])
               (if (fx= i len)
                   (reverse result)
                   (let ([value (ref arr i)])
                     (loop (fx1+ i) (if (pred value) (cons value result) result))))))]
          [(arr pred collect)
           (let ([len (length-of arr)])
             (let loop ([i 0])
               (unless (fx= i len)
                 (let ([value (ref arr i)]) (when (pred value) (collect value)))
                 (loop (fx1+ i)))))]))
      (define append-arrays
        (lambda arrays
          (pcheck ([all-bytearrays? arrays])
                  (let* ([lengths (map length-of arrays)]
                         [total (let loop ([remaining lengths] [sum 0])
                                  (if (null? remaining)
                                      sum
                                      (loop (cdr remaining) (fx+ sum (car remaining)))))]
                         [result (make-result total)])
                    (let loop ([sources arrays] [remaining-lengths lengths] [offset 0])
                      (if (null? sources)
                          result
                          (let ([bytes (bytearray-size (car sources))])
                            (bytevector-copy! (array-vec (car sources)) 0
                                              (array-vec result) (fx* offset width) bytes)
                            (loop (cdr sources) (cdr remaining-lengths)
                                  (fx+ offset (car remaining-lengths))))))))))
      (define append-arrays!
        (lambda (arr . arrays)
          (pcheck ([bytearray? arr] [all-bytearrays? arrays])
                  (replace! arr (apply append-arrays arr arrays)))))
      (define reverse-array
        (lambda (arr)
          (let* ([len (length-of arr)] [result (make-result len)])
            (let loop ([i 0])
              (if (fx= i len) result
                  (begin (set result i (ref arr (fx- len i 1))) (loop (fx1+ i))))))))
      (define reverse-array! (lambda (arr) (replace! arr (reverse-array arr))))
      (define map-array
        (case-lambda
          [(proc arr)
           (pcheck ([procedure? proc] [bytearray? arr])
                   (let* ([len (length-of arr)] [result (make-result len)])
                     (let loop ([i 0])
                       (if (fx= i len) result
                           (begin (set result i (proc (ref arr i)))
                                  (loop (fx1+ i)))))))]
          [(proc arr other)
           (pcheck ([procedure? proc] [bytearray? arr other])
                   (let* ([len (source-length arr (list other))]
                          [result (make-result len)])
                     (let loop ([i 0])
                       (if (fx= i len) result
                           (begin (set result i (proc (ref arr i) (ref other i)))
                                  (loop (fx1+ i)))))))]
          [(proc arr second third . rest)
           (let ([arrays (cons second (cons third rest))])
             (pcheck ([procedure? proc] [bytearray? arr] [all-bytearrays? arrays])
                     (let* ([sources (cons arr arrays)]
                            [len (source-length arr arrays)]
                            [result (make-result len)])
                       (let loop ([i 0])
                         (if (fx= i len) result
                             (begin (set result i (apply proc (values-at i sources)))
                                    (loop (fx1+ i))))))))]))
      (define map/i
        (case-lambda
          [(proc arr)
           (pcheck ([procedure? proc] [bytearray? arr])
                   (let* ([len (length-of arr)] [result (make-result len)])
                     (let loop ([i 0])
                       (if (fx= i len) result
                           (begin (set result i (proc i (ref arr i)))
                                  (loop (fx1+ i)))))))]
          [(proc arr other)
           (pcheck ([procedure? proc] [bytearray? arr other])
                   (let* ([len (source-length arr (list other))]
                          [result (make-result len)])
                     (let loop ([i 0])
                       (if (fx= i len) result
                           (begin (set result i (proc i (ref arr i) (ref other i)))
                                  (loop (fx1+ i)))))))]
          [(proc arr second third . rest)
           (let ([arrays (cons second (cons third rest))])
             (pcheck ([procedure? proc] [bytearray? arr] [all-bytearrays? arrays])
                     (let* ([sources (cons arr arrays)]
                            [len (source-length arr arrays)]
                            [result (make-result len)])
                       (let loop ([i 0])
                         (if (fx= i len) result
                             (begin (set result i (apply proc i (values-at i sources)))
                                    (loop (fx1+ i))))))))]))
      (define map!
        (case-lambda
          [(proc arr)
           (pcheck ([procedure? proc] [bytearray? arr])
                   (let ([len (length-of arr)])
                     (let loop ([i 0])
                       (if (fx= i len) arr
                           (begin
                             (set arr i (proc (ref arr i)))
                             (loop (fx1+ i)))))))]
          [(proc arr other)
           (pcheck ([procedure? proc] [bytearray? arr other])
                   (let ([len (source-length arr (list other))])
                     (let loop ([i 0])
                       (if (fx= i len) arr
                           (begin
                             (set arr i (proc (ref arr i) (ref other i)))
                             (loop (fx1+ i)))))))]
          [(proc arr second third . rest)
           (let ([arrays (cons second (cons third rest))])
             (pcheck ([procedure? proc] [bytearray? arr] [all-bytearrays? arrays])
                     (let* ([sources (cons arr arrays)]
                            [len (source-length arr arrays)])
                       (let loop ([i 0])
                         (if (fx= i len) arr
                             (begin
                               (set arr i (apply proc (values-at i sources)))
                               (loop (fx1+ i))))))))]))
      (define map/i!
        (case-lambda
          [(proc arr)
           (pcheck ([procedure? proc] [bytearray? arr])
                   (let ([len (length-of arr)])
                     (let loop ([i 0])
                       (if (fx= i len) arr
                           (begin
                             (set arr i (proc i (ref arr i)))
                             (loop (fx1+ i)))))))]
          [(proc arr other)
           (pcheck ([procedure? proc] [bytearray? arr other])
                   (let ([len (source-length arr (list other))])
                     (let loop ([i 0])
                       (if (fx= i len) arr
                           (begin
                             (set arr i (proc i (ref arr i) (ref other i)))
                             (loop (fx1+ i)))))))]
          [(proc arr second third . rest)
           (let ([arrays (cons second (cons third rest))])
             (pcheck ([procedure? proc] [bytearray? arr] [all-bytearrays? arrays])
                     (let* ([sources (cons arr arrays)]
                            [len (source-length arr arrays)])
                       (let loop ([i 0])
                         (if (fx= i len) arr
                             (begin
                               (set arr i (apply proc i (values-at i sources)))
                               (loop (fx1+ i))))))))]))
      (define each
        (case-lambda
          [(proc arr)
           (pcheck ([procedure? proc] [bytearray? arr])
                   (let ([len (length-of arr)])
                     (let loop ([i 0])
                       (unless (fx= i len)
                         (proc (ref arr i))
                         (loop (fx1+ i))))))]
          [(proc arr other)
           (pcheck ([procedure? proc] [bytearray? arr other])
                   (let ([len (source-length arr (list other))])
                     (let loop ([i 0])
                       (unless (fx= i len)
                         (proc (ref arr i) (ref other i))
                         (loop (fx1+ i))))))]
          [(proc arr second third . rest)
           (let ([arrays (cons second (cons third rest))])
             (pcheck ([procedure? proc] [bytearray? arr] [all-bytearrays? arrays])
                     (let* ([sources (cons arr arrays)] [len (source-length arr arrays)])
                       (let loop ([i 0])
                         (unless (fx= i len)
                           (apply proc (values-at i sources))
                           (loop (fx1+ i)))))))]))
      (define each/i
        (case-lambda
          [(proc arr)
           (pcheck ([procedure? proc] [bytearray? arr])
                   (let ([len (length-of arr)])
                     (let loop ([i 0])
                       (unless (fx= i len)
                         (proc i (ref arr i))
                         (loop (fx1+ i))))))]
          [(proc arr other)
           (pcheck ([procedure? proc] [bytearray? arr other])
                   (let ([len (source-length arr (list other))])
                     (let loop ([i 0])
                       (unless (fx= i len)
                         (proc i (ref arr i) (ref other i))
                         (loop (fx1+ i))))))]
          [(proc arr second third . rest)
           (let ([arrays (cons second (cons third rest))])
             (pcheck ([procedure? proc] [bytearray? arr] [all-bytearrays? arrays])
                     (let* ([sources (cons arr arrays)] [len (source-length arr arrays)])
                       (let loop ([i 0])
                         (unless (fx= i len)
                           (apply proc i (values-at i sources))
                           (loop (fx1+ i)))))))]))
      (define map-rev
        (case-lambda
          [(proc arr)
           (pcheck ([procedure? proc] [bytearray? arr])
                   (let* ([len (length-of arr)] [result (make-result len)])
                     (let loop ([i (fx1- len)] [output 0])
                       (if (fx< i 0) result
                           (begin (set result output (proc (ref arr i)))
                                  (loop (fx1- i) (fx1+ output)))))))]
          [(proc arr other)
           (pcheck ([procedure? proc] [bytearray? arr other])
                   (let* ([len (source-length arr (list other))]
                          [result (make-result len)])
                     (let loop ([i (fx1- len)] [output 0])
                       (if (fx< i 0) result
                           (begin (set result output (proc (ref arr i) (ref other i)))
                                  (loop (fx1- i) (fx1+ output)))))))]
          [(proc arr second third . rest)
           (let ([arrays (cons second (cons third rest))])
             (pcheck ([procedure? proc] [bytearray? arr] [all-bytearrays? arrays])
                     (let* ([sources (cons arr arrays)] [len (source-length arr arrays)]
                            [result (make-result len)])
                       (let loop ([i (fx1- len)] [output 0])
                         (if (fx< i 0) result
                             (begin (set result output (apply proc (values-at i sources)))
                                    (loop (fx1- i) (fx1+ output))))))))]))
      (define map/i-rev
        (case-lambda
          [(proc arr)
           (pcheck ([procedure? proc] [bytearray? arr])
                   (let* ([len (length-of arr)] [result (make-result len)])
                     (let loop ([i (fx1- len)] [output 0])
                       (if (fx< i 0) result
                           (begin (set result output (proc i (ref arr i)))
                                  (loop (fx1- i) (fx1+ output)))))))]
          [(proc arr other)
           (pcheck ([procedure? proc] [bytearray? arr other])
                   (let* ([len (source-length arr (list other))]
                          [result (make-result len)])
                     (let loop ([i (fx1- len)] [output 0])
                       (if (fx< i 0) result
                           (begin
                             (set result output (proc i (ref arr i) (ref other i)))
                             (loop (fx1- i) (fx1+ output)))))))]
          [(proc arr second third . rest)
           (let ([arrays (cons second (cons third rest))])
             (pcheck ([procedure? proc] [bytearray? arr] [all-bytearrays? arrays])
                     (let* ([sources (cons arr arrays)] [len (source-length arr arrays)]
                            [result (make-result len)])
                       (let loop ([i (fx1- len)] [output 0])
                         (if (fx< i 0) result
                             (begin
                               (set result output (apply proc i (values-at i sources)))
                               (loop (fx1- i) (fx1+ output))))))))]))
      (define each-rev
        (case-lambda
          [(proc arr)
           (pcheck ([procedure? proc] [bytearray? arr])
                   (let loop ([i (fx1- (length-of arr))])
                     (unless (fx< i 0) (proc (ref arr i)) (loop (fx1- i)))))]
          [(proc arr other)
           (pcheck ([procedure? proc] [bytearray? arr other])
                   (let loop ([i (fx1- (source-length arr (list other)))])
                     (unless (fx< i 0)
                       (proc (ref arr i) (ref other i))
                       (loop (fx1- i)))))]
          [(proc arr second third . rest)
           (let ([arrays (cons second (cons third rest))])
             (pcheck ([procedure? proc] [bytearray? arr] [all-bytearrays? arrays])
                     (let ([sources (cons arr arrays)])
                       (let loop ([i (fx1- (source-length arr arrays))])
                         (unless (fx< i 0)
                           (apply proc (values-at i sources))
                           (loop (fx1- i)))))))]))
      (define each/i-rev
        (case-lambda
          [(proc arr)
           (pcheck ([procedure? proc] [bytearray? arr])
                   (let loop ([i (fx1- (length-of arr))])
                     (unless (fx< i 0) (proc i (ref arr i)) (loop (fx1- i)))))]
          [(proc arr other)
           (pcheck ([procedure? proc] [bytearray? arr other])
                   (let loop ([i (fx1- (source-length arr (list other)))])
                     (unless (fx< i 0)
                       (proc i (ref arr i) (ref other i))
                       (loop (fx1- i)))))]
          [(proc arr second third . rest)
           (let ([arrays (cons second (cons third rest))])
             (pcheck ([procedure? proc] [bytearray? arr] [all-bytearrays? arrays])
                     (let ([sources (cons arr arrays)])
                       (let loop ([i (fx1- (source-length arr arrays))])
                         (unless (fx< i 0)
                           (apply proc i (values-at i sources))
                           (loop (fx1- i)))))))]))
      (define andmap-array
        (case-lambda
          [(proc arr)
           (pcheck ([procedure? proc] [bytearray? arr])
                   (let ([len (length-of arr)])
                     (let loop ([i 0])
                       (or (fx= i len) (and (proc (ref arr i)) (loop (fx1+ i)))))))]
          [(proc arr other)
           (pcheck ([procedure? proc] [bytearray? arr other])
                   (let ([len (source-length arr (list other))])
                     (let loop ([i 0])
                       (or (fx= i len)
                           (and (proc (ref arr i) (ref other i))
                                (loop (fx1+ i)))))))]
          [(proc arr second third . rest)
           (let ([arrays (cons second (cons third rest))])
             (pcheck ([procedure? proc] [bytearray? arr] [all-bytearrays? arrays])
                     (let* ([sources (cons arr arrays)] [len (source-length arr arrays)])
                       (let loop ([i 0])
                         (or (fx= i len)
                             (and (apply proc (values-at i sources))
                                  (loop (fx1+ i))))))))]))
      (define ormap-array
        (case-lambda
          [(proc arr)
           (pcheck ([procedure? proc] [bytearray? arr])
                   (let ([len (length-of arr)])
                     (let loop ([i 0])
                       (and (fx< i len) (or (proc (ref arr i)) (loop (fx1+ i)))))))]
          [(proc arr other)
           (pcheck ([procedure? proc] [bytearray? arr other])
                   (let ([len (source-length arr (list other))])
                     (let loop ([i 0])
                       (and (fx< i len)
                            (or (proc (ref arr i) (ref other i))
                                (loop (fx1+ i)))))))]
          [(proc arr second third . rest)
           (let ([arrays (cons second (cons third rest))])
             (pcheck ([procedure? proc] [bytearray? arr] [all-bytearrays? arrays])
                     (let* ([sources (cons arr arrays)] [len (source-length arr arrays)])
                       (let loop ([i 0])
                         (and (fx< i len)
                              (or (apply proc (values-at i sources))
                                  (loop (fx1+ i))))))))]))
      (define fold-left
        (case-lambda
          [(proc init arr)
           (pcheck ([procedure? proc] [bytearray? arr])
                   (let ([len (length-of arr)])
                     (let loop ([i 0] [acc init])
                       (if (fx= i len) acc
                           (loop (fx1+ i) (proc acc (ref arr i)))))))]
          [(proc init arr other)
           (pcheck ([procedure? proc] [bytearray? arr other])
                   (let ([len (source-length arr (list other))])
                     (let loop ([i 0] [acc init])
                       (if (fx= i len) acc
                           (loop (fx1+ i) (proc acc (ref arr i) (ref other i)))))))]
          [(proc init arr second third . rest)
           (let ([arrays (cons second (cons third rest))])
             (pcheck ([procedure? proc] [bytearray? arr] [all-bytearrays? arrays])
                     (let* ([sources (cons arr arrays)] [len (source-length arr arrays)])
                       (let loop ([i 0] [acc init])
                         (if (fx= i len) acc
                             (loop (fx1+ i)
                                   (apply proc acc (values-at i sources))))))))]))
      (define fold-left/i
        (case-lambda
          [(proc init arr)
           (pcheck ([procedure? proc] [bytearray? arr])
                   (let ([len (length-of arr)])
                     (let loop ([i 0] [acc init])
                       (if (fx= i len) acc
                           (loop (fx1+ i) (proc i acc (ref arr i)))))))]
          [(proc init arr other)
           (pcheck ([procedure? proc] [bytearray? arr other])
                   (let ([len (source-length arr (list other))])
                     (let loop ([i 0] [acc init])
                       (if (fx= i len) acc
                           (loop (fx1+ i)
                                 (proc i acc (ref arr i) (ref other i)))))))]
          [(proc init arr second third . rest)
           (let ([arrays (cons second (cons third rest))])
             (pcheck ([procedure? proc] [bytearray? arr] [all-bytearrays? arrays])
                     (let* ([sources (cons arr arrays)] [len (source-length arr arrays)])
                       (let loop ([i 0] [acc init])
                         (if (fx= i len) acc
                             (loop (fx1+ i)
                                   (apply proc i acc (values-at i sources))))))))]))
      (define fold-right
        (case-lambda
          [(proc init arr)
           (pcheck ([procedure? proc] [bytearray? arr])
                   (let loop ([i (fx1- (length-of arr))] [acc init])
                     (if (fx< i 0) acc (loop (fx1- i) (proc (ref arr i) acc)))))]
          [(proc init arr other)
           (pcheck ([procedure? proc] [bytearray? arr other])
                   (let loop ([i (fx1- (source-length arr (list other)))] [acc init])
                     (if (fx< i 0) acc
                         (loop (fx1- i) (proc (ref arr i) (ref other i) acc)))))]
          [(proc init arr second third . rest)
           (let ([arrays (cons second (cons third rest))])
             (pcheck ([procedure? proc] [bytearray? arr] [all-bytearrays? arrays])
                     (let ([sources (cons arr arrays)])
                       (let loop ([i (fx1- (source-length arr arrays))] [acc init])
                         (if (fx< i 0) acc
                             (loop (fx1- i)
                                   (apply proc
                                          (append (values-at i sources)
                                                  (list acc)))))))))]))
      (define fold-right/i
        (case-lambda
          [(proc init arr)
           (pcheck ([procedure? proc] [bytearray? arr])
                   (let loop ([i (fx1- (length-of arr))] [acc init])
                     (if (fx< i 0) acc
                         (loop (fx1- i) (proc i (ref arr i) acc)))))]
          [(proc init arr other)
           (pcheck ([procedure? proc] [bytearray? arr other])
                   (let loop ([i (fx1- (source-length arr (list other)))] [acc init])
                     (if (fx< i 0) acc
                         (loop (fx1- i)
                               (proc i (ref arr i) (ref other i) acc)))))]
          [(proc init arr second third . rest)
           (let ([arrays (cons second (cons third rest))])
             (pcheck ([procedure? proc] [bytearray? arr] [all-bytearrays? arrays])
                     (let ([sources (cons arr arrays)])
                       (let loop ([i (fx1- (source-length arr arrays))] [acc init])
                         (if (fx< i 0) acc
                             (loop (fx1- i)
                                   (apply proc i
                                          (append (values-at i sources)
                                                  (list acc)))))))))]))
      (define sorted?
        (case-lambda
          [(less? arr) (sorted? less? arr 0 (length-of arr))]
          [(less? arr stop) (sorted? less? arr 0 stop)]
          [(less? arr start stop)
           (pcheck ([procedure? less?] [bytearray? arr] [natural? start stop])
                   (let ([len (length-of arr)])
                     (when (fx> stop len)
                       (errorf who "stop index ~a out of bound ~a" stop len))
                     (when (fx> start stop)
                       (errorf who "start index ~a greater than stop index ~a" start stop))
                     (let loop ([i (fx1+ start)])
                       (or (fx>= i stop)
                           (and (not (less? (ref arr i) (ref arr (fx1- i))))
                                (loop (fx1+ i)))))))]))
      (define sort-array
        (case-lambda
          [(less? arr) (sort-array less? arr 0 (length-of arr))]
          [(less? arr stop) (sort-array less? arr 0 stop)]
          [(less? arr start stop)
           (pcheck ([procedure? less?] [bytearray? arr] [natural? start stop])
                   (let ([len (length-of arr)])
                     (when (fx> stop len)
                       (errorf who "stop index ~a out of bound ~a" stop len))
                     (when (fx> start stop)
                       (errorf who "start index ~a greater than stop index ~a" start stop))
                     (let ([result (make-result (fx- stop start))])
                       (let copy ([i start])
                         (unless (fx= i stop)
                           (set result (fx- i start) (ref arr i))
                           (copy (fx1+ i))))
                       ($bytearray-merge-sort! less? result 0 (fx- stop start)
                                               width ref set))))]))
      (define sort-array!
        (case-lambda
          [(less? arr) (sort-array! less? arr 0 (length-of arr))]
          [(less? arr stop) (sort-array! less? arr 0 stop)]
          [(less? arr start stop)
           (pcheck ([procedure? less?] [bytearray? arr] [natural? start stop])
                   (let ([len (length-of arr)])
                     (when (fx> stop len)
                       (errorf who "stop index ~a out of bound ~a" stop len))
                     (when (fx> start stop)
                       (errorf who "start index ~a greater than stop index ~a" start stop))
                     ($bytearray-merge-sort! less? arr start stop width ref set)))]))
      (define to-list
        (lambda (arr)
          (let ([len (length-of arr)])
            (let loop ([i 0] [result '()])
              (if (fx= i len) (reverse result)
                  (loop (fx1+ i) (cons (ref arr i) result)))))))
      (define to-iter
        (make-indexed-iter who bytearray?
                           length-of ref))
      (define to-bytevector
        (lambda (arr)
          (pcheck ([bytearray? arr])
                  (let* ([bytes (bytearray-size arr)] [result (make-bytevector bytes 0)])
                    (bytevector-copy! (array-vec arr) 0 result 0 bytes)
                    result))))
      (define iota-array
        (lambda (count)
          (pcheck ([natural? count])
                  (let ([result (make-result count)])
                    (let loop ([i 0])
                      (if (fx= i count) result
                          (begin (set result i (if (value? i) i (inexact i)))
                                 (loop (fx1+ i)))))))))
      (define nums
        (case-lambda
          [(stop) (nums zero stop one)] [(start stop) (nums start stop one)]
          [(start stop step)
           (pcheck ([number? start stop step])
                   (unless (or (and (< start stop) (> step 0))
                               (and (> start stop) (< step 0))
                               (= start stop))
                     (errorf who "invalid range: ~a, ~a, ~a" start stop step))
                   (let ([count (let loop ([value start] [count 0])
                                  (if (if (> step 0) (>= value stop) (<= value stop))
                                      count
                                      (loop (+ value step) (fx1+ count))))])
                     (let ([result (make-result count)])
                       (let loop ([i 0] [value start])
                         (if (fx= i count) result
                             (begin (check-value value) (set result i value)
                                    (loop (fx1+ i) (+ value step))))))))]))
      (vector add! add*! delete! slice slice! copy copy! push! pop! push-back! pop-back!
              filter filter! partition contains? contains/p? index-of find-index search search*
              append-arrays append-arrays! reverse-array reverse-array! map-array map/i map! map/i!
              each each/i map-rev map/i-rev each-rev each/i-rev andmap-array ormap-array
              fold-left fold-left/i fold-right fold-right/i sorted? sort-array sort-array!
              to-list to-iter to-bytevector iota-array nums)))


  #|macro:define-bytearray-procedure
  Define the width-qualified bytearray operation family for descriptor `width`.
  The `width-size` value is the number of bytes per logical value. The `ref` and `set`
  procedures have signatures `(bytearray index) -> value` and
  `(bytearray index value) -> unspecified`. Predicate `value?` has signature
  `(value) -> boolean` and validates stored values.

  Generated map and iteration procedures take a procedure and one or more equal-length
  bytearrays. A non-indexed procedure has signature `(value ...) -> value`; an indexed
  procedure has signature `(index value ...) -> value`. Iteration return values are
  unspecified. Maps return a new bytearray, except mutating maps store results directly
  in and return the first bytearray. Reverse variants visit values from the last logical
  index to the first.

  Generated predicate traversals use signature `(value ...) -> any` and short-circuit.
  Left folds use `(accumulator value ...) -> accumulator`, while right folds use
  `(value ... accumulator) -> accumulator`. Indexed folds receive `index` first.
  |#
  (define-syntax define-bytearray-procedure
    (lambda (stx)
      (syntax-case stx ()
        [(_ width width-size ref set value?)
         (let ([name (symbol->string (syntax->datum #'width))])
           (with-syntax
               ([operations ($construct-name #'width "$bytearray-" name "-operations")]
                     [n0 ($construct-name #'width "bytearray-" name "-add!")]
                     [n1 ($construct-name #'width "bytearray-" name "-add*!")]
                     [n2 ($construct-name #'width "bytearray-" name "-delete!")]
                     [n3 ($construct-name #'width "bytearray-" name "-slice")]
                     [n4 ($construct-name #'width "bytearray-" name "-slice!")]
                     [n5 ($construct-name #'width "bytearray-" name "-copy")]
                     [n6 ($construct-name #'width "bytearray-" name "-copy!")]
                     [n7 ($construct-name #'width "bytearray-" name "-push!")]
                     [n8 ($construct-name #'width "bytearray-" name "-pop!")]
                     [n9 ($construct-name #'width "bytearray-" name "-push-back!")]
                     [n10 ($construct-name #'width "bytearray-" name "-pop-back!")]
                     [n11 ($construct-name #'width "bytearray-" name "-filter")]
                     [n12 ($construct-name #'width "bytearray-" name "-filter!")]
                     [n13 ($construct-name #'width "bytearray-" name "-partition")]
                     [n14 ($construct-name #'width "bytearray-" name "-contains?")]
                     [n15 ($construct-name #'width "bytearray-" name "-contains/p?")]
                     [n16 ($construct-name #'width "bytearray-" name "-index-of")]
                     [n17 ($construct-name #'width "bytearray-" name "-find-index")]
                     [n18 ($construct-name #'width "bytearray-" name "-search")]
                     [n19 ($construct-name #'width "bytearray-" name "-search*")]
                     [n20 ($construct-name #'width "bytearray-" name "-append")]
                     [n21 ($construct-name #'width "bytearray-" name "-append!")]
                     [n22 ($construct-name #'width "bytearray-" name "-reverse")]
                     [n23 ($construct-name #'width "bytearray-" name "-reverse!")]
                     [n24 ($construct-name #'width "bytearray-" name "-map")]
                     [n25 ($construct-name #'width "bytearray-" name "-map/i")]
                     [n26 ($construct-name #'width "bytearray-" name "-map!")]
                     [n27 ($construct-name #'width "bytearray-" name "-map/i!")]
                     [n28 ($construct-name #'width "bytearray-" name "-for-each")]
                     [n29 ($construct-name #'width "bytearray-" name "-for-each/i")]
                     [n30 ($construct-name #'width "bytearray-" name "-map-rev")]
                     [n31 ($construct-name #'width "bytearray-" name "-map/i-rev")]
                     [n32 ($construct-name #'width "bytearray-" name "-for-each-rev")]
                     [n33 ($construct-name #'width "bytearray-" name "-for-each/i-rev")]
                     [n34 ($construct-name #'width "bytearray-" name "-andmap")]
                     [n35 ($construct-name #'width "bytearray-" name "-ormap")]
                     [n36 ($construct-name #'width "bytearray-" name "-fold-left")]
                     [n37 ($construct-name #'width "bytearray-" name "-fold-left/i")]
                     [n38 ($construct-name #'width "bytearray-" name "-fold-right")]
                     [n39 ($construct-name #'width "bytearray-" name "-fold-right/i")]
                     [n40 ($construct-name #'width "bytearray-" name "-sorted?")]
                     [n41 ($construct-name #'width "bytearray-" name "-sort")]
                     [n42 ($construct-name #'width "bytearray-" name "-sort!")]
                     [n43 ($construct-name #'width "bytearray-" name "->list")]
                     [n44 ($construct-name #'width "bytearray-" name "->iter")]
                     [n45 ($construct-name #'width "bytearray-" name "->bytevector")]
                     [n46 ($construct-name #'width "bytearray-" name "-iota")]
                     [n47 ($construct-name #'width "bytearray-" name "-nums")])
             #'(begin
                (define operations
                  ($make-bytearray-width-operations 'width width-size ref set value?))
                (define n0 (vector-ref operations 0))
                (define n1 (vector-ref operations 1))
                (define n2 (vector-ref operations 2))
                (define n3 (vector-ref operations 3))
                (define n4 (vector-ref operations 4))
                (define n5 (vector-ref operations 5))
                (define n6 (vector-ref operations 6))
                (define n7 (vector-ref operations 7))
                (define n8 (vector-ref operations 8))
                (define n9 (vector-ref operations 9))
                (define n10 (vector-ref operations 10))
                (define n11 (vector-ref operations 11))
                (define n12 (vector-ref operations 12))
                (define n13 (vector-ref operations 13))
                (define n14 (vector-ref operations 14))
                (define n15 (vector-ref operations 15))
                (define n16 (vector-ref operations 16))
                (define n17 (vector-ref operations 17))
                (define n18 (vector-ref operations 18))
                (define n19 (vector-ref operations 19))
                (define n20 (vector-ref operations 20))
                (define n21 (vector-ref operations 21))
                (define n22 (vector-ref operations 22))
                (define n23 (vector-ref operations 23))
                (define n24 (vector-ref operations 24))
                (define n25 (vector-ref operations 25))
                (define n26 (vector-ref operations 26))
                (define n27 (vector-ref operations 27))
                (define n28 (vector-ref operations 28))
                (define n29 (vector-ref operations 29))
                (define n30 (vector-ref operations 30))
                (define n31 (vector-ref operations 31))
                (define n32 (vector-ref operations 32))
                (define n33 (vector-ref operations 33))
                (define n34 (vector-ref operations 34))
                (define n35 (vector-ref operations 35))
                (define n36 (vector-ref operations 36))
                (define n37 (vector-ref operations 37))
                (define n38 (vector-ref operations 38))
                (define n39 (vector-ref operations 39))
                (define n40 (vector-ref operations 40))
                (define n41 (vector-ref operations 41))
                (define n42 (vector-ref operations 42))
                (define n43 (vector-ref operations 43))
                (define n44 (vector-ref operations 44))
                (define n45 (vector-ref operations 45))
                (define n46 (vector-ref operations 46))
                (define n47 (vector-ref operations 47)))))])))


  (define-bytearray-procedure u8 1 bytearray-u8-ref bytearray-u8-set! bytearray-u8-value?)
  (define-bytearray-procedure U8 1 bytearray-U8-ref bytearray-U8-set! bytearray-u8-value?)
  (define-bytearray-procedure s8 1 bytearray-s8-ref bytearray-s8-set! bytearray-s8-value?)
  (define-bytearray-procedure S8 1 bytearray-S8-ref bytearray-S8-set! bytearray-s8-value?)
  (define-bytearray-procedure u16 2 bytearray-u16-ref bytearray-u16-set! bytearray-u16-value?)
  (define-bytearray-procedure U16 2 bytearray-U16-ref bytearray-U16-set! bytearray-u16-value?)
  (define-bytearray-procedure s16 2 bytearray-s16-ref bytearray-s16-set! bytearray-s16-value?)
  (define-bytearray-procedure S16 2 bytearray-S16-ref bytearray-S16-set! bytearray-s16-value?)
  (define-bytearray-procedure u24 3 bytearray-u24-ref bytearray-u24-set! bytearray-u24-value?)
  (define-bytearray-procedure U24 3 bytearray-U24-ref bytearray-U24-set! bytearray-u24-value?)
  (define-bytearray-procedure s24 3 bytearray-s24-ref bytearray-s24-set! bytearray-s24-value?)
  (define-bytearray-procedure S24 3 bytearray-S24-ref bytearray-S24-set! bytearray-s24-value?)
  (define-bytearray-procedure u32 4 bytearray-u32-ref bytearray-u32-set! bytearray-u32-value?)
  (define-bytearray-procedure U32 4 bytearray-U32-ref bytearray-U32-set! bytearray-u32-value?)
  (define-bytearray-procedure s32 4 bytearray-s32-ref bytearray-s32-set! bytearray-s32-value?)
  (define-bytearray-procedure S32 4 bytearray-S32-ref bytearray-S32-set! bytearray-s32-value?)
  (define-bytearray-procedure u40 5 bytearray-u40-ref bytearray-u40-set! bytearray-u40-value?)
  (define-bytearray-procedure U40 5 bytearray-U40-ref bytearray-U40-set! bytearray-u40-value?)
  (define-bytearray-procedure s40 5 bytearray-s40-ref bytearray-s40-set! bytearray-s40-value?)
  (define-bytearray-procedure S40 5 bytearray-S40-ref bytearray-S40-set! bytearray-s40-value?)
  (define-bytearray-procedure u48 6 bytearray-u48-ref bytearray-u48-set! bytearray-u48-value?)
  (define-bytearray-procedure U48 6 bytearray-U48-ref bytearray-U48-set! bytearray-u48-value?)
  (define-bytearray-procedure s48 6 bytearray-s48-ref bytearray-s48-set! bytearray-s48-value?)
  (define-bytearray-procedure S48 6 bytearray-S48-ref bytearray-S48-set! bytearray-s48-value?)
  (define-bytearray-procedure u56 7 bytearray-u56-ref bytearray-u56-set! bytearray-u56-value?)
  (define-bytearray-procedure U56 7 bytearray-U56-ref bytearray-U56-set! bytearray-u56-value?)
  (define-bytearray-procedure s56 7 bytearray-s56-ref bytearray-s56-set! bytearray-s56-value?)
  (define-bytearray-procedure S56 7 bytearray-S56-ref bytearray-S56-set! bytearray-s56-value?)
  (define-bytearray-procedure u64 8 bytearray-u64-ref bytearray-u64-set! bytearray-u64-value?)
  (define-bytearray-procedure U64 8 bytearray-U64-ref bytearray-U64-set! bytearray-u64-value?)
  (define-bytearray-procedure s64 8 bytearray-s64-ref bytearray-s64-set! bytearray-s64-value?)
  (define-bytearray-procedure S64 8 bytearray-S64-ref bytearray-S64-set! bytearray-s64-value?)
  (define-bytearray-procedure fp32 4 bytearray-fp32-ref bytearray-fp32-set! flonum?)
  (define-bytearray-procedure FP32 4 bytearray-FP32-ref bytearray-FP32-set! flonum?)
  (define-bytearray-procedure fp64 8 bytearray-fp64-ref bytearray-fp64-set! flonum?)
  (define-bytearray-procedure FP64 8 bytearray-FP64-ref bytearray-FP64-set! flonum?)

  #|proc:bytearray-size
  Return the number of bytes in bytearray `arr`.
  |#
  (define bytearray-size
    (lambda (arr) (pcheck ([bytearray? arr]) ($array-size arr))))
  #|proc:bytearray-empty?
  Return whether bytearray `arr` contains no bytes.
  |#
  (define bytearray-empty?
    (lambda (arr) (pcheck ([bytearray? arr]) (fx= 0 ($array-size arr)))))
  #|proc:bytearray-ref
  Return the byte at index `index` in bytearray `arr`.
  |#
  (define bytearray-ref bytearray-u8-ref)
  #|proc:bytearray-set!
  Store byte `value` at index `index` in bytearray `arr`.
  |#
  (define bytearray-set! bytearray-u8-set!)
  #|proc:bytearray-clear!
  Remove all bytes from bytearray `arr`.
  |#
  (define bytearray-clear!
    (lambda (arr)
      (pcheck ([bytearray? arr])
              ($array-size-set! arr 0))))
  #|proc:bytearray-add!
  Add byte `value` to bytearray `arr`, optionally at logical index `index`.
  |#
  (define bytearray-add! bytearray-u8-add!)
  #|proc:bytearray-add*!
  Add the supplied byte values to bytearray `arr`, optionally at a leading index.
  |#
  (define bytearray-add*! bytearray-u8-add*!)
  (define bytearray-delete! bytearray-u8-delete!)
  (define bytearray-slice bytearray-u8-slice)
  (define bytearray-slice! bytearray-u8-slice!)
  (define bytearray-copy bytearray-u8-copy)
  (define bytearray-copy! bytearray-u8-copy!)
  (define bytearray-push! bytearray-u8-push!)
  (define bytearray-pop! bytearray-u8-pop!)
  (define bytearray-push-back! bytearray-u8-push-back!)
  (define bytearray-pop-back! bytearray-u8-pop-back!)
  (define bytearray-filter bytearray-u8-filter)
  (define bytearray-filter! bytearray-u8-filter!)
  (define bytearray-partition bytearray-u8-partition)
  (define bytearray-contains? bytearray-u8-contains?)
  (define bytearray-contains/p? bytearray-u8-contains/p?)
  (define bytearray-index-of bytearray-u8-index-of)
  (define bytearray-find-index bytearray-u8-find-index)
  (define bytearray-search bytearray-u8-search)
  (define bytearray-search* bytearray-u8-search*)
  (define bytearray-append bytearray-u8-append)
  (define bytearray-append! bytearray-u8-append!)
  (define bytearray-reverse bytearray-u8-reverse)
  (define bytearray-reverse! bytearray-u8-reverse!)
  #|proc:bytearray-map
  Apply `proc` with signature `(byte ...) -> byte` to equal-length bytearrays `arr ...`.
  Return a new bytearray containing the results.
  |#
  (define bytearray-map bytearray-u8-map)
  #|proc:bytearray-map/i
  Apply `proc` with signature `(index byte ...) -> byte` to equal-length bytearrays
  `arr ...`. Return a new bytearray containing the results.
  |#
  (define bytearray-map/i bytearray-u8-map/i)
  #|proc:bytearray-map!
  Apply `proc` with signature `(byte ...) -> byte` to equal-length bytearrays `arr ...`.
  Store results directly in the first bytearray and return it.
  |#
  (define bytearray-map! bytearray-u8-map!)
  #|proc:bytearray-map/i!
  Apply `proc` with signature `(index byte ...) -> byte` to equal-length bytearrays
  `arr ...`. Store results directly in the first bytearray and return it.
  |#
  (define bytearray-map/i! bytearray-u8-map/i!)
  #|proc:bytearray-for-each
  Call `proc` with signature `(byte ...) -> any` for equal-length bytearrays `arr ...`.
  Return unspecified values.
  |#
  (define bytearray-for-each bytearray-u8-for-each)
  #|proc:bytearray-for-each/i
  Call `proc` with signature `(index byte ...) -> any` for equal-length bytearrays
  `arr ...`. Return unspecified values.
  |#
  (define bytearray-for-each/i bytearray-u8-for-each/i)
  #|proc:bytearray-map-rev
  Apply `proc` with signature `(byte ...) -> byte` in reverse to equal-length bytearrays
  `arr ...`. Return a new bytearray in visitation order.
  |#
  (define bytearray-map-rev bytearray-u8-map-rev)
  #|proc:bytearray-map/i-rev
  Apply `proc` with signature `(index byte ...) -> byte` in reverse to equal-length
  bytearrays `arr ...`. Return a new bytearray in visitation order.
  |#
  (define bytearray-map/i-rev bytearray-u8-map/i-rev)
  #|proc:bytearray-for-each-rev
  Call `proc` with signature `(byte ...) -> any` in reverse for equal-length bytearrays
  `arr ...`. Return unspecified values.
  |#
  (define bytearray-for-each-rev bytearray-u8-for-each-rev)
  #|proc:bytearray-for-each/i-rev
  Call `proc` with signature `(index byte ...) -> any` in reverse for equal-length
  bytearrays `arr ...`. Return unspecified values.
  |#
  (define bytearray-for-each/i-rev bytearray-u8-for-each/i-rev)
  #|proc:bytearray-andmap
  Apply `proc` with signature `(byte ...) -> any` to equal-length bytearrays `arr ...`.
  Return `#f` at the first false result, or `#t` when all results are true.
  |#
  (define bytearray-andmap bytearray-u8-andmap)
  #|proc:bytearray-ormap
  Apply `proc` with signature `(byte ...) -> any` to equal-length bytearrays `arr ...`.
  Return the first true result, or `#f` when all results are false.
  |#
  (define bytearray-ormap bytearray-u8-ormap)
  #|proc:bytearray-fold-left
  Fold equal-length bytearrays `arr ...` from left to right with initial value `init`.
  Procedure `proc` has signature `(accumulator byte ...) -> accumulator`; return the result.
  |#
  (define bytearray-fold-left bytearray-u8-fold-left)
  #|proc:bytearray-fold-left/i
  Fold equal-length bytearrays `arr ...` from left to right with initial value `init`.
  Procedure `proc` has signature `(index accumulator byte ...) -> accumulator`.
  Return the final accumulator.
  |#
  (define bytearray-fold-left/i bytearray-u8-fold-left/i)
  #|proc:bytearray-fold-right
  Fold equal-length bytearrays `arr ...` from right to left with initial value `init`.
  Procedure `proc` has signature `(byte ... accumulator) -> accumulator`; return the result.
  |#
  (define bytearray-fold-right bytearray-u8-fold-right)
  #|proc:bytearray-fold-right/i
  Fold equal-length bytearrays `arr ...` from right to left with initial value `init`.
  Procedure `proc` has signature `(index byte ... accumulator) -> accumulator`.
  Return the final accumulator.
  |#
  (define bytearray-fold-right/i bytearray-u8-fold-right/i)
  (define bytearray-sorted? bytearray-u8-sorted?)
  (define bytearray-sort bytearray-u8-sort)
  (define bytearray-sort! bytearray-u8-sort!)
  (define bytearray-iota bytearray-u8-iota)
  (define bytearray-nums bytearray-u8-nums)
  (define bytearray->list bytearray-u8->list)

  (define-syntax gen-array-record-writer
    (syntax-rules ()
      [(_ arr header vref)
       (record-writer (type-descriptor arr)
                      (lambda (r p wr)
                        (display header p)
                        (let ([v (array-vec r)] [len ($array-size r)])
                          (when (fx>= len 1) (wr (vref v 0) p))
                          (when (fx> len 1)
                            (let loop ([i 1])
                              (unless (fx= i len)
                                (display " " p)
                                (wr (vref v i) p)
                                (loop (fx1+ i))))))
                        (display ")]" p)))]))

  (define-syntax gen-array-record-type-equal-procedure
    (syntax-rules ()
      [(_ arr vref)
       (record-type-equal-procedure (type-descriptor arr)
                                    (lambda (arr1 arr2 =?)
                                      (let ([len1 ($array-size arr1)] [vec1 (array-vec arr1)]
                                            [len2 ($array-size arr2)] [vec2 (array-vec arr2)])
                                        (and (fx= len1 len2)
                                             (let loop ([i 0])
                                               (if (fx= i len1)
                                                   #t
                                                   (and (=? (vref vec1 i) (vref vec2 i))
                                                        (loop (fx1+ i)))))))))]))

;;;;===----------------------------------------------------------------------===
;;;; Iterator and navigator extension registration
;;;;===----------------------------------------------------------------------===

  ;; Each family is a sibling record type, with its own size and element checks.
  ;; Pure bulk updates copy storage once; callback state belongs to each invocation.
  (define register-array-navigation!
    (lambda (predicate size ref copy setter)
      (define mutate
        (lambda (source indexes update)
          (for-each (lambda (index) (setter source index (update (ref source index)))) indexes)
          source))
      (nav-register-indexed!
       predicate size ref
       (lambda (source index value)
         (let ([result (copy source)]) (setter result index value) result))
       (lambda (source index value) (setter source index value) source)
       (lambda (source start stop step visit)
         (let loop ([index start])
           (when (< index stop)
             (visit index (ref source index))
             (loop (+ index step)))))
       (lambda (source indexes update) (mutate (copy source) indexes update))
       mutate)))

  (iter-register-source! array? array->iter)
  (iter-register-source! fxarray? fxarray->iter)
  (iter-register-source! flarray? flarray->iter)
  (iter-register-source! bytearray? bytearray->iter)

;;;;===----------------------------------------------------------------------===
;;;; Navigator extension registration
;;;;===----------------------------------------------------------------------===

  (register-array-navigation! array? array-size array-ref array-copy array-set!)
  (register-array-navigation! fxarray? fxarray-size fxarray-ref fxarray-copy fxarray-set!)
  (register-array-navigation! flarray? flarray-size flarray-ref flarray-copy flarray-set!)
  (register-array-navigation! bytearray? bytearray-size bytearray-ref bytearray-copy bytearray-set!)

  (gen-array-record-writer $array   "#[array ("   vector-ref)
  (gen-array-record-writer $fxarray "#[fxarray (" fxvector-ref)
  (gen-array-record-writer $flarray "#[flarray (" flvector-ref)
  (gen-array-record-writer $bytearray "#[bytearray (" bytevector-u8-ref)

  (gen-array-record-type-equal-procedure $array   vector-ref)
  (gen-array-record-type-equal-procedure $fxarray fxvector-ref)
  (gen-array-record-type-equal-procedure $flarray flvector-ref)
  (gen-array-record-type-equal-procedure $bytearray bytevector-u8-ref)

  )
