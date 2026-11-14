(library (cspp vector)
  (export vmap vmap/i vmap! vmap!/i vfor-each vfor-each/i
          fxvmap fxvmap/i fxvmap! fxvmap!/i fxvfor-each fxvfor-each/i
          flvmap flvmap/i flvmap! flvmap!/i flvfor-each flvfor-each/i

          vector-map/i vector-for-each/i vector-map! vector-map!/i
          fxvector-map fxvector-map/i fxvector-for-each/i fxvector-map! fxvector-map!/i
          flvector-map flvector-map/i flvector-for-each/i flvector-map! flvector-map!/i

          vslice fxvslice flvslice
          vfilter fxvfilter flvfilter
          vpartition fxvpartition flvpartition

          vormap vandmap vexists vfor-all
          fxvormap fxvandmap fxvexists fxvfor-all
          flvormap flvandmap flvexists flvfor-all

          vmemp vmember vmemq vmemv
          fxvmemp fxvmember fxvmemq fxvmemv
          flvmemp flvmember flvmemq flvmemv

          vfold-left vfold-right vfold-left/i vfold-right/i
          fxvfold-left fxvfold-right fxvfold-left/i fxvfold-right/i
          flvfold-left flvfold-right flvfold-left/i flvfold-right/i

          vscan-left-ex fxvscan-left-ex flvscan-left-ex
          vscan-left-in fxvscan-left-in flvscan-left-in
          vscan-right-ex fxvscan-right-ex flvscan-right-ex
          vscan-right-in fxvscan-right-in flvscan-right-in

          vreverse fxvreverse flvreverse
          vreverse! fxvreverse! flvreverse!
          vzip fxvzip flvzip vzipv fxvzipv flvzipv
          vshuffle  fxvshuffle  flvshuffle
          vshuffle! fxvshuffle! flvshuffle!
          vsort fxvsort flvsort
          vsort! fxvsort! flvsort!
          vsorted? fxvsorted? flvsorted?

          vcopy fxvcopy flvcopy
          vcopy! fxvcopy! flvcopy! u8vcopy!
          vsum fxvsum flvsum
          vproduct fxvproduct flvproduct
          vextreme fxvextreme flvextreme
          vmax vmin fxvmax fxvmin flvmax flvmin
          vavg fxvavg flvavg
          viota fxviota
          vnums fxvnums flvnums

          random-vector random-fxvector random-flvector

          bvmap-u8 bvmap-U8 bvmap-s8 bvmap-S8
          bvmap-u16 bvmap-U16 bvmap-s16 bvmap-S16
          bvmap-u24 bvmap-U24 bvmap-s24 bvmap-S24
          bvmap-u32 bvmap-U32 bvmap-s32 bvmap-S32
          bvmap-u40 bvmap-U40 bvmap-s40 bvmap-S40
          bvmap-u48 bvmap-U48 bvmap-s48 bvmap-S48
          bvmap-u56 bvmap-U56 bvmap-s56 bvmap-S56
          bvmap-u64 bvmap-U64 bvmap-s64 bvmap-S64
          bvmap-fp32 bvmap-FP32 bvmap-fp64 bvmap-FP64

          bvmap/i-u8 bvmap/i-U8 bvmap/i-s8 bvmap/i-S8
          bvmap/i-u16 bvmap/i-U16 bvmap/i-s16 bvmap/i-S16
          bvmap/i-u24 bvmap/i-U24 bvmap/i-s24 bvmap/i-S24
          bvmap/i-u32 bvmap/i-U32 bvmap/i-s32 bvmap/i-S32
          bvmap/i-u40 bvmap/i-U40 bvmap/i-s40 bvmap/i-S40
          bvmap/i-u48 bvmap/i-U48 bvmap/i-s48 bvmap/i-S48
          bvmap/i-u56 bvmap/i-U56 bvmap/i-s56 bvmap/i-S56
          bvmap/i-u64 bvmap/i-U64 bvmap/i-s64 bvmap/i-S64
          bvmap/i-fp32 bvmap/i-FP32 bvmap/i-fp64 bvmap/i-FP64

          bvmap!-u8 bvmap!-U8 bvmap!-s8 bvmap!-S8
          bvmap!-u16 bvmap!-U16 bvmap!-s16 bvmap!-S16
          bvmap!-u24 bvmap!-U24 bvmap!-s24 bvmap!-S24
          bvmap!-u32 bvmap!-U32 bvmap!-s32 bvmap!-S32
          bvmap!-u40 bvmap!-U40 bvmap!-s40 bvmap!-S40
          bvmap!-u48 bvmap!-U48 bvmap!-s48 bvmap!-S48
          bvmap!-u56 bvmap!-U56 bvmap!-s56 bvmap!-S56
          bvmap!-u64 bvmap!-U64 bvmap!-s64 bvmap!-S64
          bvmap!-fp32 bvmap!-FP32 bvmap!-fp64 bvmap!-FP64

          bvmap!/i-u8 bvmap!/i-U8 bvmap!/i-s8 bvmap!/i-S8
          bvmap!/i-u16 bvmap!/i-U16 bvmap!/i-s16 bvmap!/i-S16
          bvmap!/i-u24 bvmap!/i-U24 bvmap!/i-s24 bvmap!/i-S24
          bvmap!/i-u32 bvmap!/i-U32 bvmap!/i-s32 bvmap!/i-S32
          bvmap!/i-u40 bvmap!/i-U40 bvmap!/i-s40 bvmap!/i-S40
          bvmap!/i-u48 bvmap!/i-U48 bvmap!/i-s48 bvmap!/i-S48
          bvmap!/i-u56 bvmap!/i-U56 bvmap!/i-s56 bvmap!/i-S56
          bvmap!/i-u64 bvmap!/i-U64 bvmap!/i-s64 bvmap!/i-S64
          bvmap!/i-fp32 bvmap!/i-FP32 bvmap!/i-fp64 bvmap!/i-FP64

          bvfor-each-u8 bvfor-each-U8 bvfor-each-s8 bvfor-each-S8
          bvfor-each-u16 bvfor-each-U16 bvfor-each-s16 bvfor-each-S16
          bvfor-each-u24 bvfor-each-U24 bvfor-each-s24 bvfor-each-S24
          bvfor-each-u32 bvfor-each-U32 bvfor-each-s32 bvfor-each-S32
          bvfor-each-u40 bvfor-each-U40 bvfor-each-s40 bvfor-each-S40
          bvfor-each-u48 bvfor-each-U48 bvfor-each-s48 bvfor-each-S48
          bvfor-each-u56 bvfor-each-U56 bvfor-each-s56 bvfor-each-S56
          bvfor-each-u64 bvfor-each-U64 bvfor-each-s64 bvfor-each-S64
          bvfor-each-fp32 bvfor-each-FP32 bvfor-each-fp64 bvfor-each-FP64

          bvfor-each/i-u8 bvfor-each/i-U8 bvfor-each/i-s8 bvfor-each/i-S8
          bvfor-each/i-u16 bvfor-each/i-U16 bvfor-each/i-s16 bvfor-each/i-S16
          bvfor-each/i-u24 bvfor-each/i-U24 bvfor-each/i-s24 bvfor-each/i-S24
          bvfor-each/i-u32 bvfor-each/i-U32 bvfor-each/i-s32 bvfor-each/i-S32
          bvfor-each/i-u40 bvfor-each/i-U40 bvfor-each/i-s40 bvfor-each/i-S40
          bvfor-each/i-u48 bvfor-each/i-U48 bvfor-each/i-s48 bvfor-each/i-S48
          bvfor-each/i-u56 bvfor-each/i-U56 bvfor-each/i-s56 bvfor-each/i-S56
          bvfor-each/i-u64 bvfor-each/i-U64 bvfor-each/i-s64 bvfor-each/i-S64
          bvfor-each/i-fp32 bvfor-each/i-FP32 bvfor-each/i-fp64 bvfor-each/i-FP64

          bvslice-u8 bvslice-U8 bvslice-s8 bvslice-S8
          bvslice-u16 bvslice-U16 bvslice-s16 bvslice-S16
          bvslice-u24 bvslice-U24 bvslice-s24 bvslice-S24
          bvslice-u32 bvslice-U32 bvslice-s32 bvslice-S32
          bvslice-u40 bvslice-U40 bvslice-s40 bvslice-S40
          bvslice-u48 bvslice-U48 bvslice-s48 bvslice-S48
          bvslice-u56 bvslice-U56 bvslice-s56 bvslice-S56
          bvslice-u64 bvslice-U64 bvslice-s64 bvslice-S64
          bvslice-fp32 bvslice-FP32 bvslice-fp64 bvslice-FP64

          bvfilter-u8 bvfilter-U8 bvfilter-s8 bvfilter-S8
          bvfilter-u16 bvfilter-U16 bvfilter-s16 bvfilter-S16
          bvfilter-u24 bvfilter-U24 bvfilter-s24 bvfilter-S24
          bvfilter-u32 bvfilter-U32 bvfilter-s32 bvfilter-S32
          bvfilter-u40 bvfilter-U40 bvfilter-s40 bvfilter-S40
          bvfilter-u48 bvfilter-U48 bvfilter-s48 bvfilter-S48
          bvfilter-u56 bvfilter-U56 bvfilter-s56 bvfilter-S56
          bvfilter-u64 bvfilter-U64 bvfilter-s64 bvfilter-S64
          bvfilter-fp32 bvfilter-FP32 bvfilter-fp64 bvfilter-FP64

          bvpartition-u8 bvpartition-U8 bvpartition-s8 bvpartition-S8
          bvpartition-u16 bvpartition-U16 bvpartition-s16 bvpartition-S16
          bvpartition-u24 bvpartition-U24 bvpartition-s24 bvpartition-S24
          bvpartition-u32 bvpartition-U32 bvpartition-s32 bvpartition-S32
          bvpartition-u40 bvpartition-U40 bvpartition-s40 bvpartition-S40
          bvpartition-u48 bvpartition-U48 bvpartition-s48 bvpartition-S48
          bvpartition-u56 bvpartition-U56 bvpartition-s56 bvpartition-S56
          bvpartition-u64 bvpartition-U64 bvpartition-s64 bvpartition-S64
          bvpartition-fp32 bvpartition-FP32 bvpartition-fp64 bvpartition-FP64

          bvormap-u8 bvormap-U8 bvormap-s8 bvormap-S8
          bvormap-u16 bvormap-U16 bvormap-s16 bvormap-S16
          bvormap-u24 bvormap-U24 bvormap-s24 bvormap-S24
          bvormap-u32 bvormap-U32 bvormap-s32 bvormap-S32
          bvormap-u40 bvormap-U40 bvormap-s40 bvormap-S40
          bvormap-u48 bvormap-U48 bvormap-s48 bvormap-S48
          bvormap-u56 bvormap-U56 bvormap-s56 bvormap-S56
          bvormap-u64 bvormap-U64 bvormap-s64 bvormap-S64
          bvormap-fp32 bvormap-FP32 bvormap-fp64 bvormap-FP64

          bvandmap-u8 bvandmap-U8 bvandmap-s8 bvandmap-S8
          bvandmap-u16 bvandmap-U16 bvandmap-s16 bvandmap-S16
          bvandmap-u24 bvandmap-U24 bvandmap-s24 bvandmap-S24
          bvandmap-u32 bvandmap-U32 bvandmap-s32 bvandmap-S32
          bvandmap-u40 bvandmap-U40 bvandmap-s40 bvandmap-S40
          bvandmap-u48 bvandmap-U48 bvandmap-s48 bvandmap-S48
          bvandmap-u56 bvandmap-U56 bvandmap-s56 bvandmap-S56
          bvandmap-u64 bvandmap-U64 bvandmap-s64 bvandmap-S64
          bvandmap-fp32 bvandmap-FP32 bvandmap-fp64 bvandmap-FP64

          bvexists-u8 bvexists-U8 bvexists-s8 bvexists-S8
          bvexists-u16 bvexists-U16 bvexists-s16 bvexists-S16
          bvexists-u24 bvexists-U24 bvexists-s24 bvexists-S24
          bvexists-u32 bvexists-U32 bvexists-s32 bvexists-S32
          bvexists-u40 bvexists-U40 bvexists-s40 bvexists-S40
          bvexists-u48 bvexists-U48 bvexists-s48 bvexists-S48
          bvexists-u56 bvexists-U56 bvexists-s56 bvexists-S56
          bvexists-u64 bvexists-U64 bvexists-s64 bvexists-S64
          bvexists-fp32 bvexists-FP32 bvexists-fp64 bvexists-FP64

          bvfor-all-u8 bvfor-all-U8 bvfor-all-s8 bvfor-all-S8
          bvfor-all-u16 bvfor-all-U16 bvfor-all-s16 bvfor-all-S16
          bvfor-all-u24 bvfor-all-U24 bvfor-all-s24 bvfor-all-S24
          bvfor-all-u32 bvfor-all-U32 bvfor-all-s32 bvfor-all-S32
          bvfor-all-u40 bvfor-all-U40 bvfor-all-s40 bvfor-all-S40
          bvfor-all-u48 bvfor-all-U48 bvfor-all-s48 bvfor-all-S48
          bvfor-all-u56 bvfor-all-U56 bvfor-all-s56 bvfor-all-S56
          bvfor-all-u64 bvfor-all-U64 bvfor-all-s64 bvfor-all-S64
          bvfor-all-fp32 bvfor-all-FP32 bvfor-all-fp64 bvfor-all-FP64

          bvmemp-u8 bvmemp-U8 bvmemp-s8 bvmemp-S8
          bvmemp-u16 bvmemp-U16 bvmemp-s16 bvmemp-S16
          bvmemp-u24 bvmemp-U24 bvmemp-s24 bvmemp-S24
          bvmemp-u32 bvmemp-U32 bvmemp-s32 bvmemp-S32
          bvmemp-u40 bvmemp-U40 bvmemp-s40 bvmemp-S40
          bvmemp-u48 bvmemp-U48 bvmemp-s48 bvmemp-S48
          bvmemp-u56 bvmemp-U56 bvmemp-s56 bvmemp-S56
          bvmemp-u64 bvmemp-U64 bvmemp-s64 bvmemp-S64
          bvmemp-fp32 bvmemp-FP32 bvmemp-fp64 bvmemp-FP64

          bvmember-u8 bvmember-U8 bvmember-s8 bvmember-S8
          bvmember-u16 bvmember-U16 bvmember-s16 bvmember-S16
          bvmember-u24 bvmember-U24 bvmember-s24 bvmember-S24
          bvmember-u32 bvmember-U32 bvmember-s32 bvmember-S32
          bvmember-u40 bvmember-U40 bvmember-s40 bvmember-S40
          bvmember-u48 bvmember-U48 bvmember-s48 bvmember-S48
          bvmember-u56 bvmember-U56 bvmember-s56 bvmember-S56
          bvmember-u64 bvmember-U64 bvmember-s64 bvmember-S64
          bvmember-fp32 bvmember-FP32 bvmember-fp64 bvmember-FP64

          bvmemq-u8 bvmemq-U8 bvmemq-s8 bvmemq-S8
          bvmemq-u16 bvmemq-U16 bvmemq-s16 bvmemq-S16
          bvmemq-u24 bvmemq-U24 bvmemq-s24 bvmemq-S24
          bvmemq-u32 bvmemq-U32 bvmemq-s32 bvmemq-S32
          bvmemq-u40 bvmemq-U40 bvmemq-s40 bvmemq-S40
          bvmemq-u48 bvmemq-U48 bvmemq-s48 bvmemq-S48
          bvmemq-u56 bvmemq-U56 bvmemq-s56 bvmemq-S56
          bvmemq-u64 bvmemq-U64 bvmemq-s64 bvmemq-S64
          bvmemq-fp32 bvmemq-FP32 bvmemq-fp64 bvmemq-FP64

          bvmemv-u8 bvmemv-U8 bvmemv-s8 bvmemv-S8
          bvmemv-u16 bvmemv-U16 bvmemv-s16 bvmemv-S16
          bvmemv-u24 bvmemv-U24 bvmemv-s24 bvmemv-S24
          bvmemv-u32 bvmemv-U32 bvmemv-s32 bvmemv-S32
          bvmemv-u40 bvmemv-U40 bvmemv-s40 bvmemv-S40
          bvmemv-u48 bvmemv-U48 bvmemv-s48 bvmemv-S48
          bvmemv-u56 bvmemv-U56 bvmemv-s56 bvmemv-S56
          bvmemv-u64 bvmemv-U64 bvmemv-s64 bvmemv-S64
          bvmemv-fp32 bvmemv-FP32 bvmemv-fp64 bvmemv-FP64

          bvfold-left-u8 bvfold-left-U8 bvfold-left-s8 bvfold-left-S8
          bvfold-left-u16 bvfold-left-U16 bvfold-left-s16 bvfold-left-S16
          bvfold-left-u24 bvfold-left-U24 bvfold-left-s24 bvfold-left-S24
          bvfold-left-u32 bvfold-left-U32 bvfold-left-s32 bvfold-left-S32
          bvfold-left-u40 bvfold-left-U40 bvfold-left-s40 bvfold-left-S40
          bvfold-left-u48 bvfold-left-U48 bvfold-left-s48 bvfold-left-S48
          bvfold-left-u56 bvfold-left-U56 bvfold-left-s56 bvfold-left-S56
          bvfold-left-u64 bvfold-left-U64 bvfold-left-s64 bvfold-left-S64
          bvfold-left-fp32 bvfold-left-FP32 bvfold-left-fp64 bvfold-left-FP64

          bvfold-right-u8 bvfold-right-U8 bvfold-right-s8 bvfold-right-S8
          bvfold-right-u16 bvfold-right-U16 bvfold-right-s16 bvfold-right-S16
          bvfold-right-u24 bvfold-right-U24 bvfold-right-s24 bvfold-right-S24
          bvfold-right-u32 bvfold-right-U32 bvfold-right-s32 bvfold-right-S32
          bvfold-right-u40 bvfold-right-U40 bvfold-right-s40 bvfold-right-S40
          bvfold-right-u48 bvfold-right-U48 bvfold-right-s48 bvfold-right-S48
          bvfold-right-u56 bvfold-right-U56 bvfold-right-s56 bvfold-right-S56
          bvfold-right-u64 bvfold-right-U64 bvfold-right-s64 bvfold-right-S64
          bvfold-right-fp32 bvfold-right-FP32 bvfold-right-fp64 bvfold-right-FP64

          bvfold-left/i-u8 bvfold-left/i-U8 bvfold-left/i-s8 bvfold-left/i-S8
          bvfold-left/i-u16 bvfold-left/i-U16 bvfold-left/i-s16 bvfold-left/i-S16
          bvfold-left/i-u24 bvfold-left/i-U24 bvfold-left/i-s24 bvfold-left/i-S24
          bvfold-left/i-u32 bvfold-left/i-U32 bvfold-left/i-s32 bvfold-left/i-S32
          bvfold-left/i-u40 bvfold-left/i-U40 bvfold-left/i-s40 bvfold-left/i-S40
          bvfold-left/i-u48 bvfold-left/i-U48 bvfold-left/i-s48 bvfold-left/i-S48
          bvfold-left/i-u56 bvfold-left/i-U56 bvfold-left/i-s56 bvfold-left/i-S56
          bvfold-left/i-u64 bvfold-left/i-U64 bvfold-left/i-s64 bvfold-left/i-S64
          bvfold-left/i-fp32 bvfold-left/i-FP32 bvfold-left/i-fp64 bvfold-left/i-FP64

          bvfold-right/i-u8 bvfold-right/i-U8 bvfold-right/i-s8 bvfold-right/i-S8
          bvfold-right/i-u16 bvfold-right/i-U16 bvfold-right/i-s16 bvfold-right/i-S16
          bvfold-right/i-u24 bvfold-right/i-U24 bvfold-right/i-s24 bvfold-right/i-S24
          bvfold-right/i-u32 bvfold-right/i-U32 bvfold-right/i-s32 bvfold-right/i-S32
          bvfold-right/i-u40 bvfold-right/i-U40 bvfold-right/i-s40 bvfold-right/i-S40
          bvfold-right/i-u48 bvfold-right/i-U48 bvfold-right/i-s48 bvfold-right/i-S48
          bvfold-right/i-u56 bvfold-right/i-U56 bvfold-right/i-s56 bvfold-right/i-S56
          bvfold-right/i-u64 bvfold-right/i-U64 bvfold-right/i-s64 bvfold-right/i-S64
          bvfold-right/i-fp32 bvfold-right/i-FP32 bvfold-right/i-fp64 bvfold-right/i-FP64

          bvscan-left-ex-u8 bvscan-left-ex-U8 bvscan-left-ex-s8 bvscan-left-ex-S8
          bvscan-left-ex-u16 bvscan-left-ex-U16 bvscan-left-ex-s16 bvscan-left-ex-S16
          bvscan-left-ex-u24 bvscan-left-ex-U24 bvscan-left-ex-s24 bvscan-left-ex-S24
          bvscan-left-ex-u32 bvscan-left-ex-U32 bvscan-left-ex-s32 bvscan-left-ex-S32
          bvscan-left-ex-u40 bvscan-left-ex-U40 bvscan-left-ex-s40 bvscan-left-ex-S40
          bvscan-left-ex-u48 bvscan-left-ex-U48 bvscan-left-ex-s48 bvscan-left-ex-S48
          bvscan-left-ex-u56 bvscan-left-ex-U56 bvscan-left-ex-s56 bvscan-left-ex-S56
          bvscan-left-ex-u64 bvscan-left-ex-U64 bvscan-left-ex-s64 bvscan-left-ex-S64
          bvscan-left-ex-fp32 bvscan-left-ex-FP32 bvscan-left-ex-fp64 bvscan-left-ex-FP64

          bvscan-left-in-u8 bvscan-left-in-U8 bvscan-left-in-s8 bvscan-left-in-S8
          bvscan-left-in-u16 bvscan-left-in-U16 bvscan-left-in-s16 bvscan-left-in-S16
          bvscan-left-in-u24 bvscan-left-in-U24 bvscan-left-in-s24 bvscan-left-in-S24
          bvscan-left-in-u32 bvscan-left-in-U32 bvscan-left-in-s32 bvscan-left-in-S32
          bvscan-left-in-u40 bvscan-left-in-U40 bvscan-left-in-s40 bvscan-left-in-S40
          bvscan-left-in-u48 bvscan-left-in-U48 bvscan-left-in-s48 bvscan-left-in-S48
          bvscan-left-in-u56 bvscan-left-in-U56 bvscan-left-in-s56 bvscan-left-in-S56
          bvscan-left-in-u64 bvscan-left-in-U64 bvscan-left-in-s64 bvscan-left-in-S64
          bvscan-left-in-fp32 bvscan-left-in-FP32 bvscan-left-in-fp64 bvscan-left-in-FP64

          bvscan-right-ex-u8 bvscan-right-ex-U8 bvscan-right-ex-s8 bvscan-right-ex-S8
          bvscan-right-ex-u16 bvscan-right-ex-U16 bvscan-right-ex-s16 bvscan-right-ex-S16
          bvscan-right-ex-u24 bvscan-right-ex-U24 bvscan-right-ex-s24 bvscan-right-ex-S24
          bvscan-right-ex-u32 bvscan-right-ex-U32 bvscan-right-ex-s32 bvscan-right-ex-S32
          bvscan-right-ex-u40 bvscan-right-ex-U40 bvscan-right-ex-s40 bvscan-right-ex-S40
          bvscan-right-ex-u48 bvscan-right-ex-U48 bvscan-right-ex-s48 bvscan-right-ex-S48
          bvscan-right-ex-u56 bvscan-right-ex-U56 bvscan-right-ex-s56 bvscan-right-ex-S56
          bvscan-right-ex-u64 bvscan-right-ex-U64 bvscan-right-ex-s64 bvscan-right-ex-S64
          bvscan-right-ex-fp32 bvscan-right-ex-FP32 bvscan-right-ex-fp64 bvscan-right-ex-FP64

          bvscan-right-in-u8 bvscan-right-in-U8 bvscan-right-in-s8 bvscan-right-in-S8
          bvscan-right-in-u16 bvscan-right-in-U16 bvscan-right-in-s16 bvscan-right-in-S16
          bvscan-right-in-u24 bvscan-right-in-U24 bvscan-right-in-s24 bvscan-right-in-S24
          bvscan-right-in-u32 bvscan-right-in-U32 bvscan-right-in-s32 bvscan-right-in-S32
          bvscan-right-in-u40 bvscan-right-in-U40 bvscan-right-in-s40 bvscan-right-in-S40
          bvscan-right-in-u48 bvscan-right-in-U48 bvscan-right-in-s48 bvscan-right-in-S48
          bvscan-right-in-u56 bvscan-right-in-U56 bvscan-right-in-s56 bvscan-right-in-S56
          bvscan-right-in-u64 bvscan-right-in-U64 bvscan-right-in-s64 bvscan-right-in-S64
          bvscan-right-in-fp32 bvscan-right-in-FP32 bvscan-right-in-fp64 bvscan-right-in-FP64

          bvreverse-u8 bvreverse-U8 bvreverse-s8 bvreverse-S8
          bvreverse-u16 bvreverse-U16 bvreverse-s16 bvreverse-S16
          bvreverse-u24 bvreverse-U24 bvreverse-s24 bvreverse-S24
          bvreverse-u32 bvreverse-U32 bvreverse-s32 bvreverse-S32
          bvreverse-u40 bvreverse-U40 bvreverse-s40 bvreverse-S40
          bvreverse-u48 bvreverse-U48 bvreverse-s48 bvreverse-S48
          bvreverse-u56 bvreverse-U56 bvreverse-s56 bvreverse-S56
          bvreverse-u64 bvreverse-U64 bvreverse-s64 bvreverse-S64
          bvreverse-fp32 bvreverse-FP32 bvreverse-fp64 bvreverse-FP64

          bvreverse!-u8 bvreverse!-U8 bvreverse!-s8 bvreverse!-S8
          bvreverse!-u16 bvreverse!-U16 bvreverse!-s16 bvreverse!-S16
          bvreverse!-u24 bvreverse!-U24 bvreverse!-s24 bvreverse!-S24
          bvreverse!-u32 bvreverse!-U32 bvreverse!-s32 bvreverse!-S32
          bvreverse!-u40 bvreverse!-U40 bvreverse!-s40 bvreverse!-S40
          bvreverse!-u48 bvreverse!-U48 bvreverse!-s48 bvreverse!-S48
          bvreverse!-u56 bvreverse!-U56 bvreverse!-s56 bvreverse!-S56
          bvreverse!-u64 bvreverse!-U64 bvreverse!-s64 bvreverse!-S64
          bvreverse!-fp32 bvreverse!-FP32 bvreverse!-fp64 bvreverse!-FP64

          bvzip-u8 bvzip-U8 bvzip-s8 bvzip-S8
          bvzip-u16 bvzip-U16 bvzip-s16 bvzip-S16
          bvzip-u24 bvzip-U24 bvzip-s24 bvzip-S24
          bvzip-u32 bvzip-U32 bvzip-s32 bvzip-S32
          bvzip-u40 bvzip-U40 bvzip-s40 bvzip-S40
          bvzip-u48 bvzip-U48 bvzip-s48 bvzip-S48
          bvzip-u56 bvzip-U56 bvzip-s56 bvzip-S56
          bvzip-u64 bvzip-U64 bvzip-s64 bvzip-S64
          bvzip-fp32 bvzip-FP32 bvzip-fp64 bvzip-FP64

          bvzipv-u8 bvzipv-U8 bvzipv-s8 bvzipv-S8
          bvzipv-u16 bvzipv-U16 bvzipv-s16 bvzipv-S16
          bvzipv-u24 bvzipv-U24 bvzipv-s24 bvzipv-S24
          bvzipv-u32 bvzipv-U32 bvzipv-s32 bvzipv-S32
          bvzipv-u40 bvzipv-U40 bvzipv-s40 bvzipv-S40
          bvzipv-u48 bvzipv-U48 bvzipv-s48 bvzipv-S48
          bvzipv-u56 bvzipv-U56 bvzipv-s56 bvzipv-S56
          bvzipv-u64 bvzipv-U64 bvzipv-s64 bvzipv-S64
          bvzipv-fp32 bvzipv-FP32 bvzipv-fp64 bvzipv-FP64

          bvshuffle-u8 bvshuffle-U8 bvshuffle-s8 bvshuffle-S8
          bvshuffle-u16 bvshuffle-U16 bvshuffle-s16 bvshuffle-S16
          bvshuffle-u24 bvshuffle-U24 bvshuffle-s24 bvshuffle-S24
          bvshuffle-u32 bvshuffle-U32 bvshuffle-s32 bvshuffle-S32
          bvshuffle-u40 bvshuffle-U40 bvshuffle-s40 bvshuffle-S40
          bvshuffle-u48 bvshuffle-U48 bvshuffle-s48 bvshuffle-S48
          bvshuffle-u56 bvshuffle-U56 bvshuffle-s56 bvshuffle-S56
          bvshuffle-u64 bvshuffle-U64 bvshuffle-s64 bvshuffle-S64
          bvshuffle-fp32 bvshuffle-FP32 bvshuffle-fp64 bvshuffle-FP64

          bvshuffle!-u8 bvshuffle!-U8 bvshuffle!-s8 bvshuffle!-S8
          bvshuffle!-u16 bvshuffle!-U16 bvshuffle!-s16 bvshuffle!-S16
          bvshuffle!-u24 bvshuffle!-U24 bvshuffle!-s24 bvshuffle!-S24
          bvshuffle!-u32 bvshuffle!-U32 bvshuffle!-s32 bvshuffle!-S32
          bvshuffle!-u40 bvshuffle!-U40 bvshuffle!-s40 bvshuffle!-S40
          bvshuffle!-u48 bvshuffle!-U48 bvshuffle!-s48 bvshuffle!-S48
          bvshuffle!-u56 bvshuffle!-U56 bvshuffle!-s56 bvshuffle!-S56
          bvshuffle!-u64 bvshuffle!-U64 bvshuffle!-s64 bvshuffle!-S64
          bvshuffle!-fp32 bvshuffle!-FP32 bvshuffle!-fp64 bvshuffle!-FP64

          bvsort-u8 bvsort-U8 bvsort-s8 bvsort-S8
          bvsort-u16 bvsort-U16 bvsort-s16 bvsort-S16
          bvsort-u24 bvsort-U24 bvsort-s24 bvsort-S24
          bvsort-u32 bvsort-U32 bvsort-s32 bvsort-S32
          bvsort-u40 bvsort-U40 bvsort-s40 bvsort-S40
          bvsort-u48 bvsort-U48 bvsort-s48 bvsort-S48
          bvsort-u56 bvsort-U56 bvsort-s56 bvsort-S56
          bvsort-u64 bvsort-U64 bvsort-s64 bvsort-S64
          bvsort-fp32 bvsort-FP32 bvsort-fp64 bvsort-FP64

          bvsort!-u8 bvsort!-U8 bvsort!-s8 bvsort!-S8
          bvsort!-u16 bvsort!-U16 bvsort!-s16 bvsort!-S16
          bvsort!-u24 bvsort!-U24 bvsort!-s24 bvsort!-S24
          bvsort!-u32 bvsort!-U32 bvsort!-s32 bvsort!-S32
          bvsort!-u40 bvsort!-U40 bvsort!-s40 bvsort!-S40
          bvsort!-u48 bvsort!-U48 bvsort!-s48 bvsort!-S48
          bvsort!-u56 bvsort!-U56 bvsort!-s56 bvsort!-S56
          bvsort!-u64 bvsort!-U64 bvsort!-s64 bvsort!-S64
          bvsort!-fp32 bvsort!-FP32 bvsort!-fp64 bvsort!-FP64

          bvsorted?-u8 bvsorted?-U8 bvsorted?-s8 bvsorted?-S8
          bvsorted?-u16 bvsorted?-U16 bvsorted?-s16 bvsorted?-S16
          bvsorted?-u24 bvsorted?-U24 bvsorted?-s24 bvsorted?-S24
          bvsorted?-u32 bvsorted?-U32 bvsorted?-s32 bvsorted?-S32
          bvsorted?-u40 bvsorted?-U40 bvsorted?-s40 bvsorted?-S40
          bvsorted?-u48 bvsorted?-U48 bvsorted?-s48 bvsorted?-S48
          bvsorted?-u56 bvsorted?-U56 bvsorted?-s56 bvsorted?-S56
          bvsorted?-u64 bvsorted?-U64 bvsorted?-s64 bvsorted?-S64
          bvsorted?-fp32 bvsorted?-FP32 bvsorted?-fp64 bvsorted?-FP64

          bvcopy-u8 bvcopy-U8 bvcopy-s8 bvcopy-S8
          bvcopy-u16 bvcopy-U16 bvcopy-s16 bvcopy-S16
          bvcopy-u24 bvcopy-U24 bvcopy-s24 bvcopy-S24
          bvcopy-u32 bvcopy-U32 bvcopy-s32 bvcopy-S32
          bvcopy-u40 bvcopy-U40 bvcopy-s40 bvcopy-S40
          bvcopy-u48 bvcopy-U48 bvcopy-s48 bvcopy-S48
          bvcopy-u56 bvcopy-U56 bvcopy-s56 bvcopy-S56
          bvcopy-u64 bvcopy-U64 bvcopy-s64 bvcopy-S64
          bvcopy-fp32 bvcopy-FP32 bvcopy-fp64 bvcopy-FP64

          bvcopy!-u8 bvcopy!-U8 bvcopy!-s8 bvcopy!-S8
          bvcopy!-u16 bvcopy!-U16 bvcopy!-s16 bvcopy!-S16
          bvcopy!-u24 bvcopy!-U24 bvcopy!-s24 bvcopy!-S24
          bvcopy!-u32 bvcopy!-U32 bvcopy!-s32 bvcopy!-S32
          bvcopy!-u40 bvcopy!-U40 bvcopy!-s40 bvcopy!-S40
          bvcopy!-u48 bvcopy!-U48 bvcopy!-s48 bvcopy!-S48
          bvcopy!-u56 bvcopy!-U56 bvcopy!-s56 bvcopy!-S56
          bvcopy!-u64 bvcopy!-U64 bvcopy!-s64 bvcopy!-S64
          bvcopy!-fp32 bvcopy!-FP32 bvcopy!-fp64 bvcopy!-FP64

          bvsum-u8 bvsum-U8 bvsum-s8 bvsum-S8
          bvsum-u16 bvsum-U16 bvsum-s16 bvsum-S16
          bvsum-u24 bvsum-U24 bvsum-s24 bvsum-S24
          bvsum-u32 bvsum-U32 bvsum-s32 bvsum-S32
          bvsum-u40 bvsum-U40 bvsum-s40 bvsum-S40
          bvsum-u48 bvsum-U48 bvsum-s48 bvsum-S48
          bvsum-u56 bvsum-U56 bvsum-s56 bvsum-S56
          bvsum-u64 bvsum-U64 bvsum-s64 bvsum-S64
          bvsum-fp32 bvsum-FP32 bvsum-fp64 bvsum-FP64

          bvproduct-u8 bvproduct-U8 bvproduct-s8 bvproduct-S8
          bvproduct-u16 bvproduct-U16 bvproduct-s16 bvproduct-S16
          bvproduct-u24 bvproduct-U24 bvproduct-s24 bvproduct-S24
          bvproduct-u32 bvproduct-U32 bvproduct-s32 bvproduct-S32
          bvproduct-u40 bvproduct-U40 bvproduct-s40 bvproduct-S40
          bvproduct-u48 bvproduct-U48 bvproduct-s48 bvproduct-S48
          bvproduct-u56 bvproduct-U56 bvproduct-s56 bvproduct-S56
          bvproduct-u64 bvproduct-U64 bvproduct-s64 bvproduct-S64
          bvproduct-fp32 bvproduct-FP32 bvproduct-fp64 bvproduct-FP64

          bvextreme-u8 bvextreme-U8 bvextreme-s8 bvextreme-S8
          bvextreme-u16 bvextreme-U16 bvextreme-s16 bvextreme-S16
          bvextreme-u24 bvextreme-U24 bvextreme-s24 bvextreme-S24
          bvextreme-u32 bvextreme-U32 bvextreme-s32 bvextreme-S32
          bvextreme-u40 bvextreme-U40 bvextreme-s40 bvextreme-S40
          bvextreme-u48 bvextreme-U48 bvextreme-s48 bvextreme-S48
          bvextreme-u56 bvextreme-U56 bvextreme-s56 bvextreme-S56
          bvextreme-u64 bvextreme-U64 bvextreme-s64 bvextreme-S64
          bvextreme-fp32 bvextreme-FP32 bvextreme-fp64 bvextreme-FP64

          bvmax-u8 bvmax-U8 bvmax-s8 bvmax-S8
          bvmax-u16 bvmax-U16 bvmax-s16 bvmax-S16
          bvmax-u24 bvmax-U24 bvmax-s24 bvmax-S24
          bvmax-u32 bvmax-U32 bvmax-s32 bvmax-S32
          bvmax-u40 bvmax-U40 bvmax-s40 bvmax-S40
          bvmax-u48 bvmax-U48 bvmax-s48 bvmax-S48
          bvmax-u56 bvmax-U56 bvmax-s56 bvmax-S56
          bvmax-u64 bvmax-U64 bvmax-s64 bvmax-S64
          bvmax-fp32 bvmax-FP32 bvmax-fp64 bvmax-FP64

          bvmin-u8 bvmin-U8 bvmin-s8 bvmin-S8
          bvmin-u16 bvmin-U16 bvmin-s16 bvmin-S16
          bvmin-u24 bvmin-U24 bvmin-s24 bvmin-S24
          bvmin-u32 bvmin-U32 bvmin-s32 bvmin-S32
          bvmin-u40 bvmin-U40 bvmin-s40 bvmin-S40
          bvmin-u48 bvmin-U48 bvmin-s48 bvmin-S48
          bvmin-u56 bvmin-U56 bvmin-s56 bvmin-S56
          bvmin-u64 bvmin-U64 bvmin-s64 bvmin-S64
          bvmin-fp32 bvmin-FP32 bvmin-fp64 bvmin-FP64

          bvavg-u8 bvavg-U8 bvavg-s8 bvavg-S8
          bvavg-u16 bvavg-U16 bvavg-s16 bvavg-S16
          bvavg-u24 bvavg-U24 bvavg-s24 bvavg-S24
          bvavg-u32 bvavg-U32 bvavg-s32 bvavg-S32
          bvavg-u40 bvavg-U40 bvavg-s40 bvavg-S40
          bvavg-u48 bvavg-U48 bvavg-s48 bvavg-S48
          bvavg-u56 bvavg-U56 bvavg-s56 bvavg-S56
          bvavg-u64 bvavg-U64 bvavg-s64 bvavg-S64
          bvavg-fp32 bvavg-FP32 bvavg-fp64 bvavg-FP64

          bvnums-u8 bvnums-U8 bvnums-s8 bvnums-S8
          bvnums-u16 bvnums-U16 bvnums-s16 bvnums-S16
          bvnums-u24 bvnums-U24 bvnums-s24 bvnums-S24
          bvnums-u32 bvnums-U32 bvnums-s32 bvnums-S32
          bvnums-u40 bvnums-U40 bvnums-s40 bvnums-S40
          bvnums-u48 bvnums-U48 bvnums-s48 bvnums-S48
          bvnums-u56 bvnums-U56 bvnums-s56 bvnums-S56
          bvnums-u64 bvnums-U64 bvnums-s64 bvnums-S64
          bvnums-fp32 bvnums-FP32 bvnums-fp64 bvnums-FP64

          bvector-u8-map/i bvector-U8-map/i bvector-s8-map/i bvector-S8-map/i
          bvector-u16-map/i bvector-U16-map/i bvector-s16-map/i bvector-S16-map/i
          bvector-u24-map/i bvector-U24-map/i bvector-s24-map/i bvector-S24-map/i
          bvector-u32-map/i bvector-U32-map/i bvector-s32-map/i bvector-S32-map/i
          bvector-u40-map/i bvector-U40-map/i bvector-s40-map/i bvector-S40-map/i
          bvector-u48-map/i bvector-U48-map/i bvector-s48-map/i bvector-S48-map/i
          bvector-u56-map/i bvector-U56-map/i bvector-s56-map/i bvector-S56-map/i
          bvector-u64-map/i bvector-U64-map/i bvector-s64-map/i bvector-S64-map/i
          bvector-fp32-map/i bvector-FP32-map/i bvector-fp64-map/i bvector-FP64-map/i

          bvector-u8-map! bvector-U8-map! bvector-s8-map! bvector-S8-map!
          bvector-u16-map! bvector-U16-map! bvector-s16-map! bvector-S16-map!
          bvector-u24-map! bvector-U24-map! bvector-s24-map! bvector-S24-map!
          bvector-u32-map! bvector-U32-map! bvector-s32-map! bvector-S32-map!
          bvector-u40-map! bvector-U40-map! bvector-s40-map! bvector-S40-map!
          bvector-u48-map! bvector-U48-map! bvector-s48-map! bvector-S48-map!
          bvector-u56-map! bvector-U56-map! bvector-s56-map! bvector-S56-map!
          bvector-u64-map! bvector-U64-map! bvector-s64-map! bvector-S64-map!
          bvector-fp32-map! bvector-FP32-map! bvector-fp64-map! bvector-FP64-map!

          bvector-u8-map!/i bvector-U8-map!/i bvector-s8-map!/i bvector-S8-map!/i
          bvector-u16-map!/i bvector-U16-map!/i bvector-s16-map!/i bvector-S16-map!/i
          bvector-u24-map!/i bvector-U24-map!/i bvector-s24-map!/i bvector-S24-map!/i
          bvector-u32-map!/i bvector-U32-map!/i bvector-s32-map!/i bvector-S32-map!/i
          bvector-u40-map!/i bvector-U40-map!/i bvector-s40-map!/i bvector-S40-map!/i
          bvector-u48-map!/i bvector-U48-map!/i bvector-s48-map!/i bvector-S48-map!/i
          bvector-u56-map!/i bvector-U56-map!/i bvector-s56-map!/i bvector-S56-map!/i
          bvector-u64-map!/i bvector-U64-map!/i bvector-s64-map!/i bvector-S64-map!/i
          bvector-fp32-map!/i bvector-FP32-map!/i bvector-fp64-map!/i bvector-FP64-map!/i

          bvector-u8-for-each/i bvector-U8-for-each/i bvector-s8-for-each/i bvector-S8-for-each/i
          bvector-u16-for-each/i bvector-U16-for-each/i bvector-s16-for-each/i bvector-S16-for-each/i
          bvector-u24-for-each/i bvector-U24-for-each/i bvector-s24-for-each/i bvector-S24-for-each/i
          bvector-u32-for-each/i bvector-U32-for-each/i bvector-s32-for-each/i bvector-S32-for-each/i
          bvector-u40-for-each/i bvector-U40-for-each/i bvector-s40-for-each/i bvector-S40-for-each/i
          bvector-u48-for-each/i bvector-U48-for-each/i bvector-s48-for-each/i bvector-S48-for-each/i
          bvector-u56-for-each/i bvector-U56-for-each/i bvector-s56-for-each/i bvector-S56-for-each/i
          bvector-u64-for-each/i bvector-U64-for-each/i bvector-s64-for-each/i bvector-S64-for-each/i
          bvector-fp32-for-each/i bvector-FP32-for-each/i bvector-fp64-for-each/i bvector-FP64-for-each/i)
  (import (chezscheme)
          (only (cspp utils) id natural? pcheck pcheck-flvector pcheck-fxvector pcheck-natural
                             pcheck-vector)
          (cspp private common))

  (define-syntax gen-random-vector
    (syntax-rules ()
      [(_ name vmake set seed nproc)
       (define name
         (case-lambda
           [(len) (name len seed)]
           [(len n) (pcheck-natural (len)
                                    (let ([vec (vmake len)])
                                      (let loop ([i 0])
                                        (if (fx= i len)
                                            vec
                                            (begin (set vec i (random (nproc n)))
                                                   (loop (add1 i)))))))]))]))
  (gen-random-vector random-vector   make-vector   vector-set!   (most-positive-fixnum) id)
  (gen-random-vector random-fxvector make-fxvector fxvector-set! (most-positive-fixnum) id)
  (gen-random-vector random-flvector make-flvector flvector-set! (inexact (most-positive-fixnum)) inexact)

  (define all-vecs?   (lambda (x*) (andmap vector?   x*)))
  (define all-fxvecs? (lambda (x*) (andmap fxvector? x*)))
  (define all-flvecs? (lambda (x*) (andmap flvector? x*)))

  (define-syntax gen-check-length
    (syntax-rules ()
      [(_ name vlength)
       (define name
         (lambda (who . vecs)
           (unless (null? vecs)
             (unless (apply fx= (map vlength vecs))
               (errorf who "vectors are not of the same length")))))]))
  (gen-check-length check-length   vector-length)
  (gen-check-length check-fxlength fxvector-length)
  (gen-check-length check-fllength flvector-length)



  ;; A generic vector procedure definition macro
  ;; that automatically expands into procedure definitions for 3 types of vectors:
  ;; vector, fxvector, and flvector.
  ;; Both lambda and case-lambda like definition are possible.
  ;; Use any of the type flags (v fxv flv) to decide which types to specialize for.
  ;;
  ;; Type-specialized implicit bindings available in the body:
  ;; - v?: vector?, fxvector?, flvector?, depending on the given type flags
  ;; - vmake: make-vector, ...
  ;; - vref: vector-ref, ...
  ;; - vset!: vector-set!, ...
  ;; - vlength: vector-length, ...
  ;; - vpcheck: pcheck-vector, ...
  ;; - vcheck-length: check-length, check-fxlength, ...
  ;; - all-which?: all-vecs?, all-fxvecs, ...
  ;; - procname: this procedure name. Given `foo`, defines `vfoo`, `fxvfoo` and `flvfoo`, if all flags are given.
  ;;             If the given name starts with `v`, e.g, `vfoo`, then the names are still as the above.
  ;; - thisproc: bound to the current procedure, used for recursive call
  ;; - t+, t-, t*, t/: fx+, fx-, ..., in the case of fxv
  ;; - t+id t*id: the identity element in the additive/multiplicative group of fixnums/flonums
  ;; - t> t<: fx>, fx< in the case of fxv
  (define-syntax define-vector-procedure
    (lambda (stx)
      (define valid-ty*?
        (lambda (ty*)
          (if (null? (remp (lambda (x) (memq x '(v fxv flv))) ty*))
              #t
              (syntax-error ty* "define-vector-procedure: bad vector type flags (allowed ones: v fxv flv):"))))
      (define handle-ty*
        (lambda (ty*)
          (values (memq 'v ty*) (memq 'fxv ty*) (memq 'flv ty*))))
      (define get-name
        (lambda (which name)
          (let ([n (symbol->string (syntax->datum name))]
                [pre1 '((v . v) (fxv . fxv) (flv . flv))]
                [pre2 '((v . "") (fxv . fx) (flv . fl))])
            (if (eqv? (string-ref n 0) #\v)
                ($construct-name name (cdr (assoc which pre2)) n)
                ($construct-name name (cdr (assoc which pre1)) n)))))
      (syntax-case stx ()
        [(k (ty* ...) name [args body body* ...] ...)
         (and (identifier? #'name) (valid-ty*? (datum (ty* ...))))
         (let-values ([(pv? pfxv? pflv?) (handle-ty* (datum (ty* ...)))])
           (with-implicit (k v v? vmake vref vset! vlength vpcheck vcheck-length all-which? procname thisproc
                             t+ t- t* t/ t+id t*id t> t<)
             #`(begin
                 #,(if pv?
                       (with-syntax ([name (get-name 'v #'name)])
                         #`(module (name)
                             (define v     vector)
                             (define v?    vector?)
                             (define vmake make-vector)
                             (define vref  vector-ref)
                             (define vset! vector-set!)
                             (define vlength vector-length)
                             (define vcheck-length check-length)
                             (define all-which? all-vecs?)
                             (define procname 'name)
                             (define t+ +)   (define t- -)
                             (define t* *)   (define t/ /)
                             (define t+id 0) (define t*id 1)
                             (define t> >)   (define t< <)
                             (let-syntax ([vpcheck (syntax-rules () [(_ e* (... ...)) (pcheck-vector e* (... ...))])])
                               (define name
                                 (case-lambda
                                   [args body body* ...] ...))
                               (define thisproc name))))
                       ;; to create a proper definition context
                       #'(define dummy0 'dummy))
                 #,(if pfxv?
                       (with-syntax ([name (get-name 'fxv #'name)])
                         #`(module (name)
                             (define v     fxvector)
                             (define v?    fxvector?)
                             (define vmake make-fxvector)
                             (define vref  fxvector-ref)
                             (define vset! fxvector-set!)
                             (define vlength fxvector-length)
                             (define vcheck-length check-fxlength)
                             (define all-which? all-fxvecs?)
                             (define procname 'name)
                             (define t+ fx+)
                             (define t- fx-)
                             (define t* fx*)
                             (define t/ fx/)
                             (define t+id 0)
                             (define t*id 1)
                             (define t> fx>)   (define t< fx<)
                             (let-syntax ([vpcheck (syntax-rules () [(_ e* (... ...)) (pcheck-fxvector e* (... ...))])])
                               (define name
                                 (case-lambda
                                   [args body body* ...] ...))
                               (define thisproc name))))
                       #'(define dummy1 'dummy))
                 #,(if pflv?
                       (with-syntax ([name (get-name 'flv #'name)])
                         #`(module (name)
                             (define v     flvector)
                             (define v?    flvector?)
                             (define vmake make-flvector)
                             (define vref  flvector-ref)
                             (define vset! flvector-set!)
                             (define vlength flvector-length)
                             (define vcheck-length check-fllength)
                             (define all-which? all-flvecs?)
                             (define procname 'name)
                             (define t+ fl+)
                             (define t- fl-)
                             (define t* fl*)
                             (define t/ fl/)
                             (define t+id 0.0)
                             (define t*id 1.0)
                             (define t> fl>)   (define t< fl<)
                             (let-syntax ([vpcheck (syntax-rules () [(_ e* (... ...)) (pcheck-flvector e* (... ...))])])
                               (define name
                                 (case-lambda
                                   [args body body* ...] ...))
                               (define thisproc name))))
                       #'(define dummy2 'dummy)))))]
        [(k (ty* ...) (name . args) body* ...)
         (and (identifier? #'name) (valid-ty*? (datum (ty* ...))))
         (let-values ([(pv? pfxv? pflv?) (handle-ty* (datum (ty* ...)))])
           (with-implicit (k v? vmake vref vset! vlength vpcheck vcheck-length all-which? procname
                             t+ t- t* t/ t+id t*id t> t<)
             #`(begin
                 #,(if pv?
                       (with-syntax ([name (get-name 'v #'name)])
                         #`(define name
                             (lambda args
                               (let ([v     vector]
                                     [v?    vector?]
                                     [vmake make-vector]
                                     [vref  vector-ref]
                                     [vset! vector-set!]
                                     [vlength vector-length]
                                     [vcheck-length check-length]
                                     [all-which? all-vecs?]
                                     [procname 'name]
                                     [thisproc name]
                                     [t+ +] [t- -] [t* *] [t/ /] [t+id 0] [t*id 1]
                                     [t> >] [t< <])
                                 ;; this piece of syntax needs care
                                 (let-syntax ([vpcheck (syntax-rules () [(_ e* (... ...)) (pcheck-vector e* (... ...))])])
                                   body* ...)))))
                       ;; to create a proper definition context
                       #'(define dummy0 'dummy))
                 #,(if pfxv?
                       (with-syntax ([name (get-name 'fxv #'name)])
                         #`(define name
                             (lambda args
                               (let ([v     fxvector]
                                     [v?    fxvector?]
                                     [vmake make-fxvector]
                                     [vref  fxvector-ref]
                                     [vset! fxvector-set!]
                                     [vlength fxvector-length]
                                     [vcheck-length check-fxlength]
                                     [all-which? all-fxvecs?]
                                     [procname 'name]
                                     [thisproc name]
                                     [t+ fx+] [t- fx-] [t* fx*] [t/ fx/] [t+id 0] [t*id 1]
                                     [t> fx>] [t< fx<])
                                 (let-syntax ([vpcheck (syntax-rules () [(_ e* (... ...)) (pcheck-fxvector e* (... ...))])])
                                   body* ...)))))
                       #'(define dummy1 'dummy))
                 #,(if pflv?
                       (with-syntax ([name (get-name 'flv #'name)])
                         #`(define name
                             (lambda args
                               (let ([v     flvector]
                                     [v?    flvector?]
                                     [vmake make-flvector]
                                     [vref  flvector-ref]
                                     [vset! flvector-set!]
                                     [vlength flvector-length]
                                     [vcheck-length check-fllength]
                                     [all-which? all-flvecs?]
                                     [procname 'name]
                                     [thisproc name]
                                     [t+ fl+] [t- fl-] [t* fl*] [t/ fl/] [t+id 0.0] [t*id 1.0]
                                     [t> fl>] [t< fl<])
                                 (let-syntax ([vpcheck (syntax-rules () [(_ e* (... ...)) (pcheck-flvector e* (... ...))])])
                                   body* ...)))))
                       #'(define dummy2 'dummy)))))])))


  (define-vector-procedure (v fxv flv) fold-left
    ;; specialize for 3 cases to avoid `apply` overhead
    [(proc acc vec0)
     (pcheck ([procedure? proc]
              [v? vec0])
             (let ([l (vlength vec0)])
               (let loop ([i 0] [acc acc])
                 (if (fx= i l)
                     acc
                     (loop (add1 i) (proc acc (vref vec0 i)))))))]
    [(proc acc vec0 vec1)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1])
             (vcheck-length procname vec0 vec1)
             (let ([l (vlength vec0)])
               (let loop ([i 0] [acc acc])
                 (if (fx= i l)
                     acc
                     (loop (add1 i) (proc acc (vref vec0 i) (vref vec1 i)))))))]
    [(proc acc vec0 vec1 vec2)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1]
              [v? vec2])
             (vcheck-length procname vec0 vec1 vec2)
             (let ([l (vlength vec0)])
               (let loop ([i 0] [acc acc])
                 (if (fx= i l)
                     acc
                     (loop (add1 i) (proc acc (vref vec0 i) (vref vec1 i) (vref vec2 i)))))))]
    [(proc acc vec0 . vecs)
     (let* ([vec* (cons vec0 vecs)]
            [vecref* (lambda (i) (map (lambda (v) (vref v i)) vec*))])
       (pcheck ([procedure? proc]
                [all-which? vec*])
               (apply vcheck-length procname vec*)
               (let ([l (vlength vec0)])
                 (let loop ([i 0] [acc acc])
                   (if (fx= i l)
                       acc
                       (loop (add1 i) (apply proc acc (vecref* i))))))))])


  (define-vector-procedure (v fxv flv) fold-left/i
    [(proc acc vec0)
     (pcheck ([procedure? proc]
              [v? vec0])
             (let ([l (vlength vec0)])
               (let loop ([i 0] [acc acc])
                 (if (fx= i l)
                     acc
                     (loop (add1 i) (proc i acc (vref vec0 i)))))))]
    [(proc acc vec0 vec1)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1])
             (vcheck-length procname vec0 vec1)
             (let ([l (vlength vec0)])
               (let loop ([i 0] [acc acc])
                 (if (fx= i l)
                     acc
                     (loop (add1 i) (proc i acc (vref vec0 i) (vref vec1 i)))))))]
    [(proc acc vec0 vec1 vec2)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1]
              [v? vec2])
             (vcheck-length procname vec0 vec1 vec2)
             (let ([l (vlength vec0)])
               (let loop ([i 0] [acc acc])
                 (if (fx= i l)
                     acc
                     (loop (add1 i) (proc i acc (vref vec0 i) (vref vec1 i) (vref vec2 i)))))))]
    [(proc acc vec0 . vecs)
     (let* ([vec* (cons vec0 vecs)]
            [vecref* (lambda (i) (map (lambda (v) (vref v i)) vec*))])
       (pcheck ([procedure? proc]
                [all-which? vec*])
               (apply vcheck-length procname vec*)
               (let ([l (vlength vec0)])
                 (let loop ([i 0] [acc acc])
                   (if (fx= i l)
                       acc
                       (loop (add1 i) (apply proc i acc (vecref* i))))))))])


  (define-vector-procedure (v fxv flv) fold-right
    [(proc acc vec0)
     (pcheck ([procedure? proc]
              [v? vec0])
             (let ([l (vlength vec0)])
               (let loop ([i (sub1 l)] [acc acc])
                 (if (fx= i -1)
                     acc
                     (loop (sub1 i) (proc (vref vec0 i) acc))))))]
    [(proc acc vec0 vec1)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1])
             (vcheck-length procname vec0 vec1)
             (let ([l (vlength vec0)])
               (let loop ([i (sub1 l)] [acc acc])
                 (if (fx= i -1)
                     acc
                     (loop (sub1 i) (proc (vref vec0 i) (vref vec1 i) acc))))))]
    [(proc acc vec0 vec1 vec2)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1]
              [v? vec2])
             (vcheck-length procname vec0 vec1 vec2)
             (let ([l (vlength vec0)])
               (let loop ([i (sub1 l)] [acc acc])
                 (if (fx= i -1)
                     acc
                     (loop (sub1 i) (proc (vref vec0 i) (vref vec1 i) (vref vec2 i) acc))))))]
    [(proc acc vec0 . vecs)
     (let* ([vec* (cons vec0 vecs)]
            [vecref* (lambda (i) (map (lambda (v) (vref v i)) vec*))])
       (pcheck ([procedure? proc]
                [all-which? vec*])
               (apply vcheck-length procname vec*)
               (let* ([l (vlength vec0)])
                 (let loop ([i (sub1 l)] [acc acc])
                   (if (fx= i -1)
                       acc
                       (loop (sub1 i) (apply proc `(,@(vecref* i) ,acc))))))))])


  (define-vector-procedure (v fxv flv) fold-right/i
    [(proc acc vec0)
     (pcheck ([procedure? proc]
              [v? vec0])
             (let ([l (vlength vec0)])
               (let loop ([i (sub1 l)] [acc acc])
                 (if (fx= i -1)
                     acc
                     ;; this has overhead
                     (loop (sub1 i) (proc i (vref vec0 i) acc))))))]
    [(proc acc vec0 vec1)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1])
             (vcheck-length procname vec0 vec1)
             (let ([l (vlength vec0)])
               (let loop ([i (sub1 l)] [acc acc])
                 (if (fx= i -1)
                     acc
                     (loop (sub1 i) (proc i (vref vec0 i) (vref vec1 i) acc))))))]
    [(proc acc vec0 vec1 vec2)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1]
              [v? vec2])
             (vcheck-length procname vec0 vec1 vec2)
             (let ([l (vlength vec0)])
               (let loop ([i (sub1 l)] [acc acc])
                 (if (fx= i -1)
                     acc
                     (loop (sub1 i) (proc i (vref vec0 i) (vref vec1 i) (vref vec2 i) acc))))))]
    [(proc acc vec0 . vecs)
     (let* ([vec* (cons vec0 vecs)]
            [vecref* (lambda (i) (map (lambda (v) (vref v i)) vec*))])
       (pcheck ([procedure? proc]
                [all-which? vec*])
               (apply vcheck-length procname vec*)
               (let* ([l (vlength vec0)])
                 (let loop ([i (sub1 l)] [acc acc])
                   (if (fx= i -1)
                       acc
                       (loop (sub1 i) (apply proc i `(,@(vecref* i) ,acc))))))))])


  (define-vector-procedure (v fxv flv) scan-left-ex
    [(proc acc vec0)
     (pcheck ([procedure? proc] [v? vec0])
             (let* ([l (vlength vec0)] [res (vmake l)])
               (if (fx= l 0)
                   res
                   (begin
                     (vset! res 0 acc)
                     (let loop ([i 1] [acc acc])
                       (if (fx= i l)
                           res
                           (let ([nacc (proc acc (vref vec0 (fx1- i)))])
                             (vset! res i nacc)
                             (loop (fx1+ i) nacc))))))))]
    [(proc acc vec0 vec1)
     (pcheck ([procedure? proc] [v? vec0 vec1])
             (vcheck-length procname vec0 vec1)
             (let* ([l (vlength vec0)] [res (vmake l)])
               (if (fx= l 0)
                   res
                   (begin
                     (vset! res 0 acc)
                     (let loop ([i 1] [acc acc])
                       (if (fx= i l)
                           res
                           (let ([nacc (proc acc (vref vec0 (fx1- i)) (vref vec1 (fx1- i)))])
                             (vset! res i nacc)
                             (loop (fx1+ i) nacc))))))))]
    [(proc acc vec0 vec1 vec2)
     (pcheck ([procedure? proc] [v? vec0 vec1 vec2])
             (vcheck-length procname vec0 vec1 vec2)
             (let* ([l (vlength vec0)] [res (vmake l)])
               (if (fx= l 0)
                   res
                   (begin
                     (vset! res 0 acc)
                     (let loop ([i 1] [acc acc])
                       (if (fx= i l)
                           res
                           (let ([nacc (proc acc (vref vec0 (fx1- i)) (vref vec1 (fx1- i)) (vref vec2 (fx1- i)))])
                             (vset! res i nacc)
                             (loop (fx1+ i) nacc))))))))]
    [(proc acc vec0 . vecs)
     (let* ([vec* (cons vec0 vecs)]
            [vecref* (lambda (i) (map (lambda (v) (vref v i)) vec*))])
       (pcheck ([procedure? proc]
                [all-which? vec*])
               (apply vcheck-length procname vec*)
               (let* ([l (vlength vec0)] [res (vmake l)])
                 (if (fx= l 0)
                     res
                     (begin
                       (vset! res 0 acc)
                       (let loop ([i 1] [acc acc])
                         (if (fx= i l)
                             res
                             (let ([nacc (apply proc acc (vecref* (fx1- i)))])
                               (vset! res i nacc)
                               (loop (fx1+ i) nacc)))))))))])


  (define-vector-procedure (v fxv flv) scan-right-ex
    [(proc acc vec0)
     (pcheck ([procedure? proc] [v? vec0])
             (let* ([l (vlength vec0)] [res (vmake l)])
               (if (fx= l 0)
                   res
                   (begin
                     (vset! res 0 acc)
                     (let loop ([i 1] [acc acc])
                       (if (fx= i l)
                           res
                           (let ([nacc (proc (vref vec0 (fx- l i)) acc)])
                             (vset! res i nacc)
                             (loop (fx1+ i) nacc))))))))]
    [(proc acc vec0 vec1)
     (pcheck ([procedure? proc] [v? vec0 vec1])
             (vcheck-length procname vec0 vec1)
             (let* ([l (vlength vec0)] [res (vmake l)])
               (if (fx= l 0)
                   res
                   (begin
                     (vset! res 0 acc)
                     (let loop ([i 1] [acc acc])
                       (if (fx= i l)
                           res
                           (let ([nacc (proc (vref vec0 (fx- l i)) (vref vec1 (fx- l i)) acc)])
                             (vset! res i nacc)
                             (loop (fx1+ i) nacc))))))))]
    [(proc acc vec0 vec1 vec2)
     (pcheck ([procedure? proc] [v? vec0 vec1 vec2])
             (vcheck-length procname vec0 vec1 vec2)
             (let* ([l (vlength vec0)] [res (vmake l)])
               (if (fx= l 0)
                   res
                   (begin
                     (vset! res 0 acc)
                     (let loop ([i 1] [acc acc])
                       (if (fx= i l)
                           res
                           (let ([nacc (proc (vref vec0 (fx- l i)) (vref vec1 (fx- l i)) (vref vec2 (fx- l i)) acc)])
                             (vset! res i nacc)
                             (loop (fx1+ i) nacc))))))))]
    [(proc acc vec0 . vecs)
     (let* ([vec* (cons vec0 vecs)]
            [vecref* (lambda (i) (map (lambda (v) (vref v i)) vec*))])
       (pcheck ([procedure? proc]
                [all-which? vec*])
               (apply vcheck-length procname vec*)
               (let* ([l (vlength vec0)] [res (vmake l)])
                 (if (fx= l 0)
                     res
                     (begin
                       (vset! res 0 acc)
                       (let loop ([i 1] [acc acc])
                         (if (fx= i l)
                             res
                             (let ([nacc (apply proc `(,@(vecref* (fx- l i)) ,acc))])
                               (vset! res i nacc)
                               (loop (fx1+ i) nacc)))))))))])


  (define-vector-procedure (v fxv flv) scan-left-in
    [(proc acc vec0)
     (pcheck ([procedure? proc] [v? vec0])
             (let* ([l (vlength vec0)] [res (vmake l)])
               (let loop ([i 0] [acc acc])
                 (if (fx= i l)
                     res
                     (let ([nacc (proc acc (vref vec0 i))])
                       (vset! res i nacc)
                       (loop (fx1+ i) nacc))))))]
    [(proc acc vec0 vec1)
     (pcheck ([procedure? proc] [v? vec0 vec1])
             (vcheck-length procname vec0 vec1)
             (let* ([l (vlength vec0)] [res (vmake l)])
               (let loop ([i 0] [acc acc])
                 (if (fx= i l)
                     res
                     (let ([nacc (proc acc (vref vec0 i) (vref vec1 i))])
                       (vset! res i nacc)
                       (loop (fx1+ i) nacc))))))]
    [(proc acc vec0 vec1 vec2)
     (pcheck ([procedure? proc] [v? vec0 vec1 vec2])
             (vcheck-length procname vec0 vec1 vec2)
             (let* ([l (vlength vec0)] [res (vmake l)])
               (let loop ([i 0] [acc acc])
                 (if (fx= i l)
                     res
                     (let ([nacc (proc acc (vref vec0 i) (vref vec1 i) (vref vec2 i))])
                       (vset! res i nacc)
                       (loop (fx1+ i) nacc))))))]
    [(proc acc vec0 . vecs)
     (let* ([vec* (cons vec0 vecs)]
            [vecref* (lambda (i) (map (lambda (v) (vref v i)) vec*))])
       (pcheck ([procedure? proc]
                [all-which? vec*])
               (apply vcheck-length procname vec*)
               (let* ([l (vlength vec0)] [res (vmake l)])
                 (let loop ([i 0] [acc acc])
                   (if (fx= i l)
                       res
                       (let ([nacc (apply proc acc (vecref* i))])
                         (vset! res i nacc)
                         (loop (fx1+ i) nacc)))))))])


  (define-vector-procedure (v fxv flv) scan-right-in
    [(proc acc vec0)
     (pcheck ([procedure? proc] [v? vec0])
             (let* ([l-1 (fx1- (vlength vec0))] [res (vmake (fx1+ l-1))])
               (let loop ([i 0] [acc acc])
                 (if (fx> i l-1)
                     res
                     (let ([nacc (proc (vref vec0 (fx- l-1 i)) acc)])
                       (vset! res i nacc)
                       (loop (fx1+ i) nacc))))))]
    [(proc acc vec0 vec1)
     (pcheck ([procedure? proc] [v? vec0 vec1])
             (vcheck-length procname vec0 vec1)
             (let* ([l-1 (fx1- (vlength vec0))] [res (vmake (fx1+ l-1))])
               (let loop ([i 0] [acc acc])
                 (if (fx> i l-1)
                     res
                     (let ([nacc (proc (vref vec0 (fx- l-1 i)) (vref vec1 (fx- l-1 i)) acc)])
                       (vset! res i nacc)
                       (loop (fx1+ i) nacc))))))]
    [(proc acc vec0 vec1 vec2)
     (pcheck ([procedure? proc] [v? vec0 vec1 vec2])
             (vcheck-length procname vec0 vec1 vec2)
             (let* ([l-1 (fx1- (vlength vec0))] [res (vmake (fx1+ l-1))])
               (let loop ([i 0] [acc acc])
                 (if (fx> i l-1)
                     res
                     (let ([nacc (proc (vref vec0 (fx- l-1 i)) (vref vec1 (fx- l-1 i)) (vref vec2 (fx- l-1 i)) acc)])
                       (vset! res i nacc)
                       (loop (fx1+ i) nacc))))))]
    [(proc acc vec0 . vecs)
     (let* ([vec* (cons vec0 vecs)]
            [vecref* (lambda (i) (map (lambda (v) (vref v i)) vec*))])
       (pcheck ([procedure? proc]
                [all-which? vec*])
               (apply vcheck-length procname vec*)
               (let* ([l-1 (fx1- (vlength vec0))] [res (vmake (fx1+ l-1))])
                 (let loop ([i 0] [acc acc])
                   (if (fx> i l-1)
                       res
                       (let ([nacc (apply proc `(,@(vecref* (fx- l-1 i)),acc))])
                         (vset! res i nacc)
                         (loop (fx1+ i) nacc)))))))])


  (define-vector-procedure (v fxv flv) andmap
    [(proc vec0)
     (pcheck ([procedure? proc]
              [v? vec0])
             (let ([l (vlength vec0)])
               (let loop ([i 0])
                 (if (fx= i l)
                     #t
                     (if (proc (vref vec0 i))
                         (loop (add1 i))
                         #f)))))]
    [(proc vec0 vec1)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1])
             (vcheck-length procname vec0 vec1)
             (let ([l (vlength vec0)])
               (let loop ([i 0])
                 (if (fx= i l)
                     #t
                     (if (proc (vref vec0 i) (vref vec1 i))
                         (loop (add1 i))
                         #f)))))]
    [(proc vec0 vec1 vec2)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1]
              [v? vec2])
             (vcheck-length procname vec0 vec1 vec2)
             (let ([l (vlength vec0)])
               (let loop ([i 0])
                 (if (fx= i l)
                     #t
                     (if (proc (vref vec0 i) (vref vec1 i) (vref vec2 i))
                         (loop (add1 i))
                         #f)))))]
    [(proc vec0 . vecs)
     (let* ([vec* (cons vec0 vecs)]
            [vecref* (lambda (i) (map (lambda (v) (vref v i)) vec*))])
       (pcheck ([procedure? proc]
                [all-which? vec*])
               (apply vcheck-length procname vec*)
               (let ([l (vlength vec0)])
                 (let loop ([i 0])
                   (if (fx= i l)
                       #t
                       (if (apply proc (vecref* i))
                           (loop (add1 i))
                           #f))))))])


  (define-vector-procedure (v fxv flv) ormap
    [(proc vec0)
     (pcheck ([procedure? proc]
              [v? vec0])
             (let ([l (vlength vec0)])
               (let loop ([i 0])
                 (if (fx= i l)
                     #f
                     (if (proc (vref vec0 i))
                         #t
                         (loop (add1 i)))))))]
    [(proc vec0 vec1)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1])
             (vcheck-length procname vec0 vec1)
             (let ([l (vlength vec0)])
               (let loop ([i 0])
                 (if (fx= i l)
                     #f
                     (if (proc (vref vec0 i) (vref vec1 i))
                         #t
                         (loop (add1 i)))))))]
    [(proc vec0 vec1 vec2)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1]
              [v? vec2])
             (vcheck-length procname vec0 vec1 vec2)
             (let ([l (vlength vec0)])
               (let loop ([i 0])
                 (if (fx= i l)
                     #f
                     (if (proc (vref vec0 i) (vref vec1 i) (vref vec2 i))
                         #t
                         (loop (add1 i)))))))]
    [(proc vec0 . vecs)
     (let* ([vec* (cons vec0 vecs)]
            [vecref* (lambda (i) (map (lambda (v) (vref v i)) vec*))])
       (pcheck ([procedure? proc]
                [all-which? vec*])
               (apply vcheck-length procname vec*)
               (let ([l (vlength vec0)])
                 (let loop ([i 0])
                   (if (fx= i l)
                       #f
                       (if (apply proc (vecref* i))
                           #t
                           (loop (add1 i))))))))])


  (define-vector-procedure (fxv flv) vector-map
    [(proc vec0)
     (pcheck ([procedure? proc]
              [v? vec0])
             (let* ([l (vlength vec0)]
                    [res (vmake l)])
               (let loop ([i 0])
                 (if (fx= i l)
                     res
                     (begin (vset! res i (proc (vref vec0 i)))
                            (loop (add1 i)))))))]
    [(proc vec0 vec1)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1])
             (vcheck-length procname vec0 vec1)
             (let* ([l (vlength vec0)]
                    [res (vmake l)])
               (let loop ([i 0])
                 (if (fx= i l)
                     res
                     (begin (vset! res i (proc (vref vec0 i) (vref vec1 i)))
                            (loop (add1 i)))))))]
    [(proc vec0 vec1 vec2)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1]
              [v? vec2])
             (vcheck-length procname vec0 vec1 vec2)
             (let* ([l (vlength vec0)]
                    [res (vmake l)])
               (let loop ([i 0])
                 (if (fx= i l)
                     res
                     (begin (vset! res i (proc (vref vec0 i) (vref vec1 i) (vref vec2 i)))
                            (loop (add1 i)))))))]
    [(proc vec0 . vecs)
     (let* ([vec* (cons vec0 vecs)]
            [vecref* (lambda (i) (map (lambda (v) (vref v i)) vec*))])
       (pcheck ([procedure? proc]
                [all-which? vec*])
               (apply vcheck-length procname vec*)
               (let* ([l (vlength vec0)]
                      [res (vmake l)])
                 (let loop ([i 0])
                   (if (fx= i l)
                       res
                       (begin (vset! res i (apply proc (vecref* i)))
                              (loop (add1 i))))))))])


  (define-vector-procedure (fxv flv) vector-for-each
    [(proc vec0)
     (pcheck ([procedure? proc]
              [v? vec0])
             (let ([l (vlength vec0)])
               (let loop ([i 0])
                 (unless (fx= i l)
                   (proc (vref vec0 i))
                   (loop (add1 i))))))]
    [(proc vec0 vec1)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1])
             (vcheck-length procname vec0 vec1)
             (let ([l (vlength vec0)])
               (let loop ([i 0])
                 (unless (fx= i l)
                   (proc (vref vec0 i) (vref vec1 i))
                   (loop (add1 i))))))]
    [(proc vec0 vec1 vec2)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1]
              [v? vec2])
             (vcheck-length procname vec0 vec1 vec2)
             (let ([l (vlength vec0)])
               (let loop ([i 0])
                 (unless (fx= i l)
                   (proc (vref vec0 i) (vref vec1 i) (vref vec2 i))
                   (loop (add1 i))))))]
    [(proc vec0 . vecs)
     (let* ([vec* (cons vec0 vecs)]
            [vecref* (lambda (i) (map (lambda (v) (vref v i)) vec*))])
       (pcheck ([procedure? proc]
                [all-which? vec*])
               (apply vcheck-length procname vec*)
               (let ([l (vlength vec0)])
                 (let loop ([i 0])
                   (unless (fx= i l)
                     (apply proc (vecref* i))
                     (loop (add1 i)))))))])


  (define-vector-procedure (v fxv flv) vector-map/i
    [(proc vec0)
     (pcheck ([procedure? proc]
              [v? vec0])
             (let* ([l (vlength vec0)]
                    [res (vmake l)])
               (let loop ([i 0])
                 (if (fx= i l)
                     res
                     (begin (vset! res i (proc i (vref vec0 i)))
                            (loop (add1 i)))))))]
    [(proc vec0 vec1)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1])
             (vcheck-length procname vec0 vec1)
             (let* ([l (vlength vec0)]
                    [res (vmake l)])
               (let loop ([i 0])
                 (if (fx= i l)
                     res
                     (begin (vset! res i (proc i (vref vec0 i) (vref vec1 i)))
                            (loop (add1 i)))))))]
    [(proc vec0 vec1 vec2)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1]
              [v? vec2])
             (vcheck-length procname vec0 vec1 vec2)
             (let* ([l (vlength vec0)]
                    [res (vmake l)])
               (let loop ([i 0])
                 (if (fx= i l)
                     res
                     (begin (vset! res i (proc i (vref vec0 i) (vref vec1 i) (vref vec2 i)))
                            (loop (add1 i)))))))]
    [(proc vec0 . vecs)
     (let* ([vec* (cons vec0 vecs)]
            [vecref* (lambda (i) (map (lambda (v) (vref v i)) vec*))])
       (pcheck ([procedure? proc]
                [all-which? vec*])
               (apply vcheck-length procname vec*)
               (let* ([l (vlength vec0)]
                      [res (vmake l)])
                 (let loop ([i 0])
                   (if (fx= i l)
                       res
                       (begin (vset! res i (apply proc i (vecref* i)))
                              (loop (add1 i))))))))])


  (define-vector-procedure (v fxv flv) vector-for-each/i
    [(proc vec0)
     (pcheck ([procedure? proc]
              [v? vec0])
             (let ([l (vlength vec0)])
               (let loop ([i 0])
                 (unless (fx= i l)
                   (proc i (vref vec0 i))
                   (loop (add1 i))))))]
    [(proc vec0 vec1)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1])
             (vcheck-length procname vec0 vec1)
             (let ([l (vlength vec0)])
               (let loop ([i 0])
                 (unless (fx= i l)
                   (proc i (vref vec0 i) (vref vec1 i))
                   (loop (add1 i))))))]
    [(proc vec0 vec1 vec2)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1]
              [v? vec2])
             (vcheck-length procname vec0 vec1 vec2)
             (let ([l (vlength vec0)])
               (let loop ([i 0])
                 (unless (fx= i l)
                   (proc i (vref vec0 i) (vref vec1 i) (vref vec2 i))
                   (loop (add1 i))))))]
    [(proc vec0 . vecs)
     (let* ([vec* (cons vec0 vecs)]
            [vecref* (lambda (i) (map (lambda (v) (vref v i)) vec*))])
       (pcheck ([procedure? proc]
                [all-which? vec*])
               (apply vcheck-length procname vec*)
               (let ([l (vlength vec0)])
                 (let loop ([i 0])
                   (unless (fx= i l)
                     (apply proc i (vecref* i))
                     (loop (add1 i)))))))])


  (define-vector-procedure (v fxv flv) vector-map!
    [(proc vec0)
     (pcheck ([procedure? proc]
              [v? vec0])
             (let* ([l (vlength vec0)])
               (let loop ([i 0])
                 (if (fx= i l)
                     vec0
                     (begin (vset! vec0 i (proc (vref vec0 i)))
                            (loop (add1 i)))))))]
    [(proc vec0 vec1)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1])
             (vcheck-length procname vec0 vec1)
             (let* ([l (vlength vec0)])
               (let loop ([i 0])
                 (if (fx= i l)
                     vec0
                     (begin (vset! vec0 i (proc (vref vec0 i) (vref vec1 i)))
                            (loop (add1 i)))))))]
    [(proc vec0 vec1 vec2)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1]
              [v? vec2])
             (vcheck-length procname vec0 vec1 vec2)
             (let* ([l (vlength vec0)])
               (let loop ([i 0])
                 (if (fx= i l)
                     vec0
                     (begin (vset! vec0 i (proc (vref vec0 i) (vref vec1 i) (vref vec2 i)))
                            (loop (add1 i)))))))]
    [(proc vec0 . vecs)
     (let* ([vec* (cons vec0 vecs)]
            [vecref* (lambda (i) (map (lambda (v) (vref v i)) vec*))])
       (pcheck ([procedure? proc]
                [all-which? vec*])
               (apply vcheck-length procname vec*)
               (let* ([l (vlength vec0)])
                 (let loop ([i 0])
                   (if (fx= i l)
                       vec0
                       (begin (vset! vec0 i (apply proc (vecref* i)))
                              (loop (add1 i))))))))])


  (define-vector-procedure (v fxv flv) vector-map!/i
    [(proc vec0)
     (pcheck ([procedure? proc]
              [v? vec0])
             (let* ([l (vlength vec0)])
               (let loop ([i 0])
                 (if (fx= i l)
                     vec0
                     (begin (vset! vec0 i (proc i (vref vec0 i)))
                            (loop (add1 i)))))))]
    [(proc vec0 vec1)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1])
             (vcheck-length procname vec0 vec1)
             (let* ([l (vlength vec0)])
               (let loop ([i 0])
                 (if (fx= i l)
                     vec0
                     (begin (vset! vec0 i (proc i (vref vec0 i) (vref vec1 i)))
                            (loop (add1 i)))))))]
    [(proc vec0 vec1 vec2)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1]
              [v? vec2])
             (vcheck-length procname vec0 vec1 vec2)
             (let* ([l (vlength vec0)])
               (let loop ([i 0])
                 (if (fx= i l)
                     vec0
                     (begin (vset! vec0 i (proc i (vref vec0 i) (vref vec1 i) (vref vec2 i)))
                            (loop (add1 i)))))))]
    [(proc vec0 . vecs)
     (let* ([vec* (cons vec0 vecs)]
            [vecref* (lambda (i) (map (lambda (v) (vref v i)) vec*))])
       (pcheck ([procedure? proc]
                [all-which? vec*])
               (apply vcheck-length procname vec*)
               (let* ([l (vlength vec0)])
                 (let loop ([i 0])
                   (if (fx= i l)
                       vec0
                       (begin (vset! vec0 i (apply proc i (vecref* i)))
                              (loop (add1 i))))))))])


  #|doc
  Return a slice, or subvector of the original vector.

  Meanings of `start`, `end` and `step` are the same as in list:slice.

  If the indices are out of range in any way, an empty vector is returned.
  |#
  (define-vector-procedure (v fxv flv) slice
    [(vec end) (thisproc vec 0 end 1)]
    [(vec start end) (thisproc vec start end 1)]
    [(vec start end step)
     (vpcheck (vec)
              (pcheck ([fixnum? start end step])
                      (when (fx= step 0) (errorf procname "step cannot be 0"))
                      (let* ([len (vlength vec)]
                             [s (let ([s (if (fx>= start 0) start (fx+ len start))])
                                  (cond [(fx< s 0) 0]
                                        [(fx> s len) (fx1- len)]
                                        [else s]))]
                             [e (let ([e (if (fx>= end 0) end (fx+ len end))])
                                  (cond [(fx<= e -1) -1]
                                        [(fx>= e len) len]
                                        [else e]))])
                        (if (fx= len 0)
                            (vmake 0)
                            (cond [(and (fx< s e) (fx> step 0))
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
                                  [else (vmake 0)])))))])


  #|doc
  Return a newly allocated vector consisting of the items of `vec` in reverse order.
  |#
  (define-vector-procedure (v fxv flv)
    (reverse vec)
    (vpcheck (vec)
             (let* ([l (vlength vec)] [res (vmake l)])
               (let loop ([i 0])
                 (if (fx= i l)
                     res
                     (begin (vset! res i (vref vec (fx- l 1 i)))
                            (loop (add1 i))))))))


  #|doc
  Reverse the items in the vector in place, then return the vector.
  |#
  (define-vector-procedure (v fxv flv)
    (reverse! vec)
    (vpcheck (vec)
             (let ([l (vlength vec)])
               (unless (fx= l 0)
                 (let ([mid (fx/ l 2)])
                   (let loop ([i 0])
                     (unless (fx= i mid)
                       (let* ([righti (fx- l 1 i)]
                              [left  (vref vec i)]
                              [right (vref vec righti)])
                         (vset! vec i right)
                         (vset! vec righti left)
                         (loop (add1 i)))))))
               vec)))


  #|doc
  Return a generic vector whose items are items from each vector zipped into a list.
  |#
  (define-vector-procedure (v fxv flv) zip
    [(vec0 vec1)
     (vpcheck (vec0 vec1)
              (vcheck-length procname vec0 vec1)
              (let* ([l (vlength vec0)]
                     ;; has to be a vector
                     [res (make-vector l)])
                (let loop ([i 0])
                  (if (fx= i l)
                      res
                      (begin (vector-set! res i (list (vref vec0 i) (vref vec1 i)))
                             (loop (add1 i)))))))]
    [(vec0 vec1 . vecs)
     (let ([vec* (cons* vec0 vec1 vecs)])
       (pcheck ([all-which? vec*])
               (apply vcheck-length procname vec0 vec1 vec*)
               (let* ([l (vlength vec0)]
                      [res (make-vector l)])
                 (let loop ([i 0])
                   (if (fx= i l)
                       res
                       (begin (vector-set! res i (map (lambda (v) (vref v i)) vec*))
                              (loop (add1 i))))))))])


  #|doc
  Return a generic vector whose items are items from each vector zipped into a vector,
  hence the `v` in the end.
  |#
  (define-vector-procedure (v fxv flv) zipv
    [(vec0 vec1)
     (vpcheck (vec0 vec1)
              (vcheck-length procname vec0 vec1)
              (let* ([l (vlength vec0)]
                     [res (make-vector l)])
                (let loop ([i 0])
                  (if (fx= i l)
                      res
                      (begin (vector-set! res i (v (vref vec0 i) (vref vec1 i)))
                             (loop (add1 i)))))))]
    [(vec0 vec1 . vecs)
     (let ([vec* (cons* vec0 vec1 vecs)])
       (pcheck ([all-which? vec*])
               (apply vcheck-length procname vec0 vec1 vec*)
               (let* ([l (vlength vec0)]
                      [res (make-vector l)])
                 (let loop ([i 0])
                   (if (fx= i l)
                       res
                       (let* ([subl (length vec*)] [item (vmake subl)])
                         (let lp ([j 0] [vs vec*])
                           (if (fx= j subl)
                               (begin (vector-set! res i item)
                                      (loop (add1 i)))
                               (begin (vset! item j (vref (car vs) i))
                                      (lp (add1 j) (cdr vs)))))))))))])


  #|doc
  Return a new vector randomly permutated from `vec`.
  |#
  (define-vector-procedure (v fxv flv)
    (shuffle vec)
    (vpcheck (vec)
             (let* ([len (vlength vec)]
                    [newv (vmake len)])
               (define swap!
                 (lambda (i j)
                   (let ([x (vref newv i)] [y (vref newv j)])
                     (vset! newv i y) (vset! newv j x))))
               (let loop ([i 0])
                 (unless (fx= i len)
                   (vset! newv i (vref vec i))
                   (loop (fx1+ i))))
               (cond [(>= len 3)
                      (let loop ([i (fx1- len)])
                        (unless (fx= i 0)
                          (swap! i (random i))
                          (loop (fx1- i))))]
                     [(= len 2) (swap! 0 1)])
               newv)))


  #|doc
  Shuffle the vector in place.
  |#
  ;; Fisher–Yates shuffle
  (define-vector-procedure (v fxv flv)
    (shuffle! vec)
    (vpcheck (vec)
             (define swap!
               (lambda (i j)
                 (let ([x (vref vec i)] [y (vref vec j)])
                   (vset! vec i y) (vset! vec j x))))
             (let ([len (vlength vec)])
               (cond [(>= len 3)
                      (let loop ([i (fx1- len)])
                        (unless (fx= i 0)
                          (swap! i (random i))
                          (loop (fx1- i))))]
                     [(= len 2) (swap! 0 1)]))
             vec))


  #|doc
  The procedure applies `pred` to each element of the  vector `vec` and
  returns a new vector of the same type consisting of the elements of `vec`
  for which `pred` returned a true value.
  |#
  (define-vector-procedure (v fxv flv)
    (filter pred vec)
    (vpcheck (vec)
             (pcheck ([procedure? pred])
                     ;; use a bitvec to record index of nice items
                     (let* ([len (vlength vec)]
                            [bitvec (make-bytevector (add1 (div len 8)) 0)])
                       (define setbit!
                         (lambda (x)
                           (let-values ([(i1 i2) (div-and-mod x 8)])
                             (let ([v (bytevector-u8-ref bitvec i1)])
                               (bytevector-u8-set! bitvec i1 (logbit1 i2 v))))))
                       (define bitset?
                         (lambda (x)
                           (let-values ([(i1 i2) (div-and-mod x 8)])
                             (let ([v (bytevector-u8-ref bitvec i1)])
                               (logbit? i2 v)))))
                       (define popcount
                         (lambda ()
                           (let loop ([i 0] [p 0])
                             (if (fx= i (bytevector-length bitvec))
                                 p
                                 (loop (fx1+ i)
                                       (+ p (fxpopcount (bytevector-u8-ref bitvec i))))))))
                       (let loop ([i 0])
                         (if (fx= i len)
                             ;; fill newvec
                             (let ([newvec (vmake (popcount))])
                               (let lp ([i 0] [j 0])
                                 (if (fx= i len)
                                     newvec
                                     (if (bitset? i)
                                         (begin
                                           (vset! newvec j (vref vec i))
                                           (lp (fx1+ i) (fx1+ j)))
                                         (lp (fx1+ i) j)))))
                             (begin
                               (when (pred (vref vec i))
                                 (setbit! i))
                               (loop (fx1+ i)))))))))


  #|doc
  Apply the unary procedure `pred` to each element of the vector `vec`, and  return two values,
  the first one a vector of the elements of `vec` for which `pred` returned a true value,
  and the second a vector of the elements of `vec` for which `pred` returned #f.

  The elements of the result vectors are in the same order as they appear in the input vector.
  |#
  (define-vector-procedure (v fxv flv)
    (partition pred vec)
    (vpcheck (vec)
             (pcheck ([procedure? pred])
                     ;; use a bitvec to record index of nice items
                     (let* ([len (vlength vec)]
                            [bitvec (make-bytevector (add1 (div len 8)) 0)])
                       (define setbit!
                         (lambda (x)
                           (let-values ([(i1 i2) (div-and-mod x 8)])
                             (let ([v (bytevector-u8-ref bitvec i1)])
                               (bytevector-u8-set! bitvec i1 (logbit1 i2 v))))))
                       (define bitset?
                         (lambda (x)
                           (let-values ([(i1 i2) (div-and-mod x 8)])
                             (let ([v (bytevector-u8-ref bitvec i1)])
                               (logbit? i2 v)))))
                       (define popcount
                         (lambda ()
                           (let loop ([i 0] [p 0])
                             (if (fx= i (bytevector-length bitvec))
                                 p
                                 (loop (fx1+ i)
                                       (+ p (fxpopcount (bytevector-u8-ref bitvec i))))))))
                       (let loop ([i 0])
                         (if (fx= i len)
                             ;; fill newvec
                             (let ([newvecT (vmake (popcount))]
                                   [newvecF (vmake (- len (popcount)))])
                               (let lp ([i 0] [jT 0] [jF 0])
                                 (if (fx= i len)
                                     (values newvecT newvecF)
                                     (if (bitset? i)
                                         (begin
                                           (vset! newvecT jT (vref vec i))
                                           (lp (fx1+ i) (fx1+ jT) jF))
                                         (begin
                                           (vset! newvecF jF (vref vec i))
                                           (lp (fx1+ i) jT (fx1+ jF)))))))
                             (begin
                               (when (pred (vref vec i))
                                 (setbit! i))
                               (loop (fx1+ i)))))))))


  ;; right bounds are all exclusive
  (define-syntax swap!
    (syntax-rules ()
      [(_ vec vref vset! i j)
       (let ([t (vref vec i)])
         (vset! vec i (vref vec j))
         (vset! vec j t))]))

  (define $sorted?
    (lambda (vec <? start stop vref)
      (let loop ([i start])
        (if (fx= i (fx1- stop))
            #t
            (and (<? (vref vec i) (vref vec (fx1+ i)))
                 (loop (fx1+ i)))))))

  (define (insertion-sort! <? vec start stop vlength vref vset!)
    (let lp1 ([i (fx1+ start)])
      (when (fx< i stop)
        (let lp2 ([j i])
          (when (fx> j start)
            (when (<? (vref vec j) (vref vec (fx1- j)))
              (swap! vec vref vset! (fx1- j) j)
              (lp2 (fx1- j)))))
        (lp1 (fx1+ i)))))

  ;; TODO optimize for duplicate values
  (define (quick-sort! <? vec start stop vlength vref vset!)
    (define (partition! lo hi)
      (let ([pivot (vref vec (fx1- hi))])
        (let loop ([i (fx1- lo)] [j lo])
          (if (fx>= j (fx1- hi))
              (begin (swap! vec vref vset! (fx1+ i) (fx1- hi))
                     (fx1+ i))
              (if (<? (vref vec j) pivot)
                  (begin (swap! vec vref vset! (fx1+ i) j)
                         (loop (fx1+ i) (fx1+ j)))
                  (loop i (fx1+ j)))))))
    (when (fx< start stop)
      (let ([p (partition! start stop)])
        (quick-sort! <? vec start    p    vlength vref vset!)
        (quick-sort! <? vec (fx1+ p) stop vlength vref vset!))))

  ;; TODO optimize
  (define (merge-sort! <? vec start stop vlength vref vset! vmake vcopy!)
    (define vtemp (vmake (- stop start)))
    (define (merge! start mid stop)
      (let ([len (fx- stop start)])
        (let loop ([i start] [j mid] [k 0])
          (cond [(fx= i mid)
                 (let lp ([j j] [k k])
                   (when (fx< j stop)
                     (vset! vtemp k (vref vec j))
                     (lp (fx1+ j) (fx1+ k))))]
                [(fx= j stop)
                 (let lp ([i i] [k k])
                   (when (fx< i mid)
                     (vset! vtemp k (vref vec i))
                     (lp (fx1+ i) (fx1+ k))))]
                [(<? (vref vec i) (vref vec j))
                 (vset! vtemp k (vref vec i))
                 (loop (fx1+ i) j (fx1+ k))]
                [else
                 (vset! vtemp k (vref vec j))
                 (loop i (fx1+ j) (fx1+ k))]))
        (vcopy! vtemp 0 vec start len)))
    (define (msort! start stop)
      (when (fx< (fx1+ start) stop) ;; exclude one item case
        (if (fx<= (fx- stop start) 32)
            (insertion-sort! <? vec start stop vlength vref vset!)
            (let ([mid (fx+ start (fx/ (fx- stop start) 2))])
              (msort! start mid)
              (msort! mid stop)
              (merge! start mid stop)))))
    (msort! start stop))


  #|doc
  The `*vsort` procedures uses the binary comparison procedure `<?` to sort the vector `vec`.
  If only two arguments are given, the entire vector is sorted;
  If the `stop` argument is given, the range from 0 to `stop-1` in `vec` is sorted;
  If both `start` and `stop` are given, the range from `start` to `stop-1` in `vec` is sorted.

  `start` and `stop` must satisfy the requirement that `0 <= start <= stop <= length of vec`.

  The `*vsort` procedures return the sorted vector or the subvector.
  |#
  (define-vector-procedure (v fxv flv) sort
    [(<? vec)
     (vpcheck (vec)
              (thisproc <? vec 0 (vlength vec)))]
    [(<? vec stop)
     (vpcheck (vec)
              (thisproc <? vec 0 stop))]
    [(<? vec start stop)
     (vpcheck (vec)
              (pcheck ([procedure? <?] [natural? start stop])
                      (let ([len (vlength vec)])
                        (when (fx> stop len)
                          (errorf procname "stop index ~a out of bound ~a" stop len))
                        (when (fx> start stop)
                          (errorf procname "start index ~a greater than stop index ~a" start stop))
                        (let ([newv (vmake (fx- stop start))]
                              [vcopy! (cond [(fxvector? vec) fxvcopy!]
                                            [(flvector? vec) flvcopy!]
                                            [else vcopy!])])
                          (vcopy! vec start newv 0 (fx- stop start))
                          (if (fx<= (vlength newv) 64)
                              (insertion-sort! <? newv 0 (vlength newv) vlength vref vset!)
                              (merge-sort!     <? newv 0 (vlength newv) vlength vref vset! vmake vcopy!))
                          newv))))])


  #|doc
  The `*vsort!` procedures uses the binary comparison procedure `<?` to sort the vector `vec`, in place.
  If only two arguments are given, the entire vector is sorted;
  If the `stop` argument is given, the range from 0 to `stop-1` in `vec` is sorted;
  If both `start` and `stop` are given, the range from `start` to `stop-1` in `vec` is sorted.

  `start` and `stop` must satisfy the requirement that `0 <= start <= stop <= length of vec`.
  |#
  (define-vector-procedure (v fxv flv) sort!
    [(<? vec)
     (vpcheck (vec)
              (thisproc <? vec 0 (vlength vec)))]
    [(<? vec stop)
     (vpcheck (vec)
              (thisproc <? vec 0 stop))]
    [(<? vec start stop)
     (vpcheck (vec)
              (pcheck ([procedure? <?] [natural? start stop])
                      (let ([len (vlength vec)])
                        (when (fx> stop len)
                          (errorf procname "stop index ~a out of bound ~a" stop len))
                        (when (fx> start stop)
                          (errorf procname "start index ~a greater than stop index ~a" start stop))
                        (let ([vcopy! (cond [(fxvector? vec) fxvcopy!]
                                            [(flvector? vec) flvcopy!]
                                            [else vcopy!])])
                          (if (fx<= (vlength vec) 64)
                              (insertion-sort! <? vec start stop vlength vref vset!)
                              (merge-sort!     <? vec start stop vlength vref vset! vmake vcopy!))))))])


  #|doc
  Check whether the given vector is sorted according to predicate `<?`.
  If `stop` is given, only the items with indices [0, stop) are checked;
  If both `start` and `stop` are given, only the items with indices [start, stop) are checked.

  `start` and `stop` must satisfy the requirement that `0 <= start <= stop <= length of vector`.
  |#
  (define-vector-procedure (v fxv flv) sorted?
    [(<? vec)
     (vpcheck (vec)
              (thisproc <? vec 0 (vlength vec)))]
    [(<? vec stop)
     (vpcheck (vec)
              (thisproc <? vec 0 stop))]
    [(<? vec start stop)
     (vpcheck (vec)
              (pcheck ([procedure? <?] [natural? start stop])
                      (let ([len (vlength vec)])
                        (when (fx> stop len)
                          (errorf procname "stop index ~a out of bound ~a" stop len))
                        (when (fx> start stop)
                          (errorf procname "start index ~a greater than stop index ~a" start stop))
                        (if (fx<= len 1)
                            #t
                            ($sorted? vec <? start stop vref)))))])


  #|doc
  Return the index of the first vector item such that (proc item) => #t,
  otherwise return #f.
  |#
  (define-vector-procedure (v fxv flv)
    (memp proc vec)
    (pcheck ([procedure? proc] [v? vec])
            (let ([l (vlength vec)])
              (let loop ([i 0])
                (if (fx= i l)
                    #f
                    (if (proc (vref vec i))
                        i
                        (loop (add1 i))))))))


  (define-syntax gen-vmember
    (lambda (stx)
      (syntax-case stx ()
        [(_ procname pcheck memp)
         (identifier? #'procname)
         #'(define procname
             (lambda (obj vec)
               (pcheck (vec)
                       (memp (lambda (x) (eqv? x obj)) vec))))])))
  (gen-vmember vmember   pcheck-vector   vmemp)
  (gen-vmember fxvmember pcheck-fxvector fxvmemp)
  (gen-vmember flvmember pcheck-flvector flvmemp)


  (define-syntax gen-vmemq
    (lambda (stx)
      (syntax-case stx ()
        [(_ procname pcheck memp)
         (identifier? #'procname)
         #'(define procname
             (lambda (obj vec)
               (pcheck (vec)
                       (memp (lambda (x) (eqv? x obj)) vec))))])))
  (gen-vmemq vmemq   pcheck-vector   vmemp)
  (gen-vmemq fxvmemq pcheck-fxvector fxvmemp)
  (gen-vmemq flvmemq pcheck-flvector flvmemp)


  (define-syntax gen-vmemv
    (lambda (stx)
      (syntax-case stx ()
        [(_ procname pcheck memp)
         (identifier? #'procname)
         #'(define procname
             (lambda (obj vec)
               (pcheck (vec)
                       (memp (lambda (x) (eqv? x obj)) vec))))])))
  (gen-vmemv vmemv   pcheck-vector   vmemp)
  (gen-vmemv fxvmemv pcheck-fxvector fxvmemp)
  (gen-vmemv flvmemv pcheck-flvector flvmemp)


  #|doc
  Similar to `iota`, generate a vector of the corresponding type
  consisting of integers from 0 to n-1.
  |#
  (define-vector-procedure (v fxv)
    (iota n)
    (pcheck ([natural? n])
            (let ([v (vmake n)])
              (let loop ([i 0])
                (if (fx= i n)
                    v
                    (begin (vset! v i i)
                           (loop (fx1+ i))))))))


  #|doc
  Generate a vector of the corresponding type consisting of numbers
  start, start+step*1, start+step*2, ...

  `start`, `stop` and `step` must be numbers that meet the following requirements:
  If `start` is less than `stop`, then `step` must be greater than 0,
  in which case the sequence terminates when the value is greater than or equal to `stop`;
  If `start` is greater than `stop`, then `step` must be less than 0,
  in which case the sequence terminates when the value is less than or equal to `stop`.
  |#
  (define-vector-procedure (v) nums
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
                         vec
                         (begin (vset! vec i x)
                                (loop (fx1+ i) (+ x step))))))
                 (errorf procname "invalid range: ~a, ~a, ~a" start stop step)))])


  #|doc
  Generate a vector of the corresponding type consisting of integers
  start, start+step*1, start+step*2, ...

  `start`, `stop` and `step` must be integers that meet the following requirements:
  If `start` is less than `stop`, then `step` must be greater than 0,
  in which case the sequence terminates when the value is greater than or equal to `stop`;
  If `start` is greater than `stop`, then `step` must be less than 0,
  in which case the sequence terminates when the value is less than or equal to `stop`.
  |#
  (define-vector-procedure (fxv) nums
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
                         vec
                         (begin (vset! vec i x)
                                (loop (fx1+ i) (+ x step))))))
                 (errorf procname "invalid range: ~a, ~a, ~a" start stop step)))])


  #|doc
  Generate a flvector consisting of flonums
  start, start+step*1, start+step*2, ...

  `start`, `stop` and `step` must be flonums that meet the following requirements:
  If `start` is less than `stop`, then `step` must be greater than 0,
  in which case the sequence terminates when the value is greater than or equal to `stop`;
  If `start` is greater than `stop`, then `step` must be less than 0,
  in which case the sequence terminates when the value is less than or equal to `stop`.
  |#
  (define-vector-procedure (flv) nums
    [(stop) (thisproc 0 stop 1)]
    [(start stop) (thisproc start stop 1)]
    [(start stop step)
     (pcheck ([flonum? start stop step])
             (if (or (and (<= start stop) (> step 0.0))
                     (and (>= start stop) (< step 0.0)))
                 (let* ([len (flonum->fixnum (ceiling (/ (- stop start) step)))]
                        [vec (vmake len 0.0)])
                   (let loop ([i 0] [x start])
                     (if (fx= i len)
                         vec
                         (begin (vset! vec i x)
                                (loop (fx1+ i) (+ x step))))))
                 (errorf procname "invalid range: ~a, ~a, ~a" start stop step)))])


  ;; aliases
  (define vmap    vector-map)
  (define vmap/i  vector-map/i)
  (define vmap!   vector-map!)
  (define vmap!/i vector-map!/i)
  ;;(define vsort   vector-sort)
  ;;(define vsort!  vector-sort!)
  #|proc:vfor-each
  Apply `proc` to corresponding elements of one or more vectors.
  The vectors must have equal lengths; return an unspecified value.
  |#
  (define vfor-each
    (case-lambda
      [(proc vec0)
       (pcheck ([procedure? proc] [vector? vec0])
               (vector-for-each proc vec0))]
      [(proc vec0 vec1)
       (pcheck ([procedure? proc] [vector? vec0] [vector? vec1])
               (check-length 'vfor-each vec0 vec1)
               (vector-for-each proc vec0 vec1))]
      [(proc vec0 . vecs)
       (apply vector-for-each proc vec0 vecs)]))
  (define vfor-each/i vector-for-each/i)

  (define fxvmap    fxvector-map)
  (define fxvmap/i  fxvector-map/i)
  (define fxvmap!   fxvector-map!)
  (define fxvmap!/i fxvector-map!/i)
  ;; (define vsort   vector-sort)
  ;; (define vsort!  vector-sort!)
  (define fxvfor-each   fxvector-for-each)
  (define fxvfor-each/i fxvector-for-each/i)

  (define flvmap    flvector-map)
  (define flvmap/i  flvector-map/i)
  (define flvmap!   flvector-map!)
  (define flvmap!/i flvector-map!/i)
  ;; (define vsort   vector-sort)
  ;; (define vsort!  vector-sort!)
  (define flvfor-each   flvector-for-each)
  (define flvfor-each/i flvector-for-each/i)

  (define vcopy   vector-copy)
  (define fxvcopy fxvector-copy)
  (define flvcopy flvector-copy)

  (define vcopy!   vector-copy!)
  (define fxvcopy! fxvector-copy!)
  (define flvcopy! flvector-copy!)
  (define u8vcopy! bytevector-copy!)

  (define vfor-all   vandmap)
  (define fxvfor-all fxvandmap)
  (define flvfor-all flvandmap)

  (define vexists   vormap)
  (define fxvexists fxvormap)
  (define flvexists flvormap)


;;;; arithmetics

  ;; TODO optimize for loop and use that

  (define-vector-procedure (v fxv flv)
    (sum vec)
    (vpcheck (vec)
             (let ([l (vlength vec)])
               (let loop ([i 0] [acc t+id])
                 (if (fx= i l)
                     acc
                     (loop (add1 i) (t+ acc (vref vec i))))))))


  (define-vector-procedure (v fxv flv)
    (product vec)
    (vpcheck (vec)
             (let ([l (vlength vec)])
               (let loop ([i 0] [acc t*id])
                 (if (fx= i l)
                     acc
                     (loop (add1 i) (t* acc (vref vec i))))))))


  (define-vector-procedure (v fxv flv)
    (avg vec)
    (vpcheck (vec)
             (let ([l (vlength vec)])
               (let loop ([i 0] [acc t+id])
                 (if (fx= i l)
                     (/ acc i)
                     (loop (add1 i) (t+ acc (vref vec i))))))))


  #|doc
  Return the extreme value in a vector, using `f` as the comparison function.
  |#
  (define-vector-procedure (v fxv flv)
    (extreme f vec)
    (vpcheck (vec)
             (let ([l (vlength vec)])
               (if (fx= l 0)
                   #f
                   (let loop ([i 1] [res (vref vec 0)])
                     (if (fx= i l)
                         res
                         (let ([x (vref vec i)])
                           (loop (add1 i) (if (f x res) x res)))))))))


  #|proc:vmax
  Return the greatest item in nonempty vector `vec`, or `#f` for an empty vector.
  |#
  (define vmax   (lambda (vec) (vextreme   >   vec)))
  #|proc:fxvmax
  Return the greatest fixnum in `vec`, or `#f` for an empty fxvector.
  |#
  (define fxvmax (lambda (vec) (fxvextreme fx> vec)))
  #|proc:flvmax
  Return the greatest flonum in `vec`, or `#f` for an empty flvector.
  |#
  (define flvmax (lambda (vec) (flvextreme fl> vec)))
  #|proc:vmin
  Return the least item in nonempty vector `vec`, or `#f` for an empty vector.
  |#
  (define vmin   (lambda (vec) (vextreme   <   vec)))
  #|proc:fxvmin
  Return the least fixnum in `vec`, or `#f` for an empty fxvector.
  |#
  (define fxvmin (lambda (vec) (fxvextreme fx< vec)))
  #|proc:flvmin
  Return the least flonum in `vec`, or `#f` for an empty flvector.
  |#
  (define flvmin (lambda (vec) (flvextreme fl< vec)))

  (define $bvector-length
    (lambda (who bytes width)
      (pcheck ([bytevector? bytes])
              (let ([length (bytevector-length bytes)])
                (unless (fx= 0 (modulo length width))
                  (errorf who "bytevector length ~a is not aligned to width ~a" length width))
                (fx/ length width)))))

  (define $bvector-merge-sort!
    (lambda (less? bytes start stop width ref set)
      (let ([scratch (make-bytevector (fx* width (fx- stop start)) 0)])
        (define copy-value!
          (lambda (from to)
            (set scratch (fx* (fx- to start) width)
                 (ref bytes (fx* from width)))))
        (define merge!
          (lambda (left middle right)
            (let loop ([i left] [j middle] [k left])
              (cond
                [(and (fx< i middle) (fx< j right))
                 (if (less? (ref bytes (fx* j width))
                            (ref bytes (fx* i width)))
                     (begin (copy-value! j k) (loop i (fx1+ j) (fx1+ k)))
                     (begin (copy-value! i k) (loop (fx1+ i) j (fx1+ k))))]
                [(fx< i middle)
                 (copy-value! i k)
                 (loop (fx1+ i) j (fx1+ k))]
                [(fx< j right)
                 (copy-value! j k)
                 (loop i (fx1+ j) (fx1+ k))]
                [else
                 (let loop-back ([n left])
                   (unless (fx= n right)
                     (set bytes (fx* n width)
                          (ref scratch (fx* (fx- n start) width)))
                     (loop-back (fx1+ n))))]))))
        (define sort-range!
          (lambda (left right)
            (when (fx< (fx1+ left) right)
              (let ([middle (fx/ (fx+ left right) 2)])
                (sort-range! left middle)
                (sort-range! middle right)
                (merge! left middle right)))))
        (sort-range! start stop)
        bytes)))

  (define $make-bvector-operations
    (lambda (who width ref set value?)
      (define zero (if (value? 0) 0 0.0))
      (define one (if (value? 1) 1 1.0))
      (define length-of (lambda (bytes) ($bvector-length who bytes width)))
      (define make-result
        (lambda (length) (make-bytevector (fx* width length) 0)))
      (define value-at
        (lambda (bytes index) (ref bytes (fx* index width))))
      (define store!
        (lambda (bytes index value)
          (unless (value? value)
            (errorf who "value is invalid for the selected width: ~a" value))
          (set bytes (fx* index width) value)))
      (define all-bytevectors?
        (lambda (values) (andmap bytevector? values)))
      (define source-length
        (lambda (bytes . rest)
          (pcheck ([bytevector? bytes] [all-bytevectors? rest])
                  (let ([length (length-of bytes)])
                    (for-each
                     (lambda (source)
                       (unless (fx= length (length-of source))
                         (errorf who "bytevectors differ in logical length")))
                     rest)
                    length))))
      (define values-at
        (lambda (index sources)
          (map (lambda (source) (value-at source index)) sources)))
      (define replace!
        (lambda (target source)
          (unless (fx= (bytevector-length target) (bytevector-length source))
            (errorf who "result length differs from target length"))
          (bytevector-copy! source 0 target 0 (bytevector-length source))
          target))
      (define map-values
        (case-lambda
          [(proc bytes)
           (pcheck ([procedure? proc] [bytevector? bytes])
                   (let* ([length (length-of bytes)] [result (make-result length)])
                     (let loop ([i 0])
                       (if (fx= i length) result
                           (begin (store! result i (proc (value-at bytes i)))
                                  (loop (fx1+ i)))))))]
          [(proc bytes other)
           (pcheck ([procedure? proc] [bytevector? bytes other])
                   (let* ([length (source-length bytes other)]
                          [result (make-result length)])
                     (let loop ([i 0])
                       (if (fx= i length) result
                           (begin
                             (store! result i
                                     (proc (value-at bytes i) (value-at other i)))
                             (loop (fx1+ i)))))))]
          [(proc bytes second third . rest)
           (let ([bytevectors (cons second (cons third rest))])
             (pcheck ([procedure? proc] [bytevector? bytes]
                      [all-bytevectors? bytevectors])
                     (let* ([sources (cons bytes bytevectors)]
                            [length (apply source-length bytes bytevectors)]
                            [result (make-result length)])
                       (let loop ([i 0])
                         (if (fx= i length) result
                             (begin (store! result i (apply proc (values-at i sources)))
                                    (loop (fx1+ i))))))))]))
      (define map/i
        (case-lambda
          [(proc bytes)
           (pcheck ([procedure? proc] [bytevector? bytes])
                   (let* ([length (length-of bytes)] [result (make-result length)])
                     (let loop ([i 0])
                       (if (fx= i length) result
                           (begin (store! result i (proc i (value-at bytes i)))
                                  (loop (fx1+ i)))))))]
          [(proc bytes other)
           (pcheck ([procedure? proc] [bytevector? bytes other])
                   (let* ([length (source-length bytes other)]
                          [result (make-result length)])
                     (let loop ([i 0])
                       (if (fx= i length) result
                           (begin
                             (store! result i
                                     (proc i (value-at bytes i) (value-at other i)))
                             (loop (fx1+ i)))))))]
          [(proc bytes second third . rest)
           (let ([bytevectors (cons second (cons third rest))])
             (pcheck ([procedure? proc] [bytevector? bytes]
                      [all-bytevectors? bytevectors])
                     (let* ([sources (cons bytes bytevectors)]
                            [length (apply source-length bytes bytevectors)]
                            [result (make-result length)])
                       (let loop ([i 0])
                         (if (fx= i length) result
                             (begin
                               (store! result i (apply proc i (values-at i sources)))
                               (loop (fx1+ i))))))))]))
      (define map!
        (case-lambda
          [(proc bytes)
           (pcheck ([procedure? proc] [bytevector? bytes])
                   (let ([length (length-of bytes)])
                     (let loop ([i 0])
                       (if (fx= i length) bytes
                           (begin
                             (store! bytes i (proc (value-at bytes i)))
                             (loop (fx1+ i)))))))]
          [(proc bytes other)
           (pcheck ([procedure? proc] [bytevector? bytes other])
                   (let ([length (source-length bytes other)])
                     (let loop ([i 0])
                       (if (fx= i length) bytes
                           (begin
                             (store! bytes i
                                     (proc (value-at bytes i) (value-at other i)))
                             (loop (fx1+ i)))))))]
          [(proc bytes second third . rest)
           (let ([bytevectors (cons second (cons third rest))])
             (pcheck ([procedure? proc] [bytevector? bytes]
                      [all-bytevectors? bytevectors])
                     (let* ([sources (cons bytes bytevectors)]
                            [length (apply source-length bytes bytevectors)])
                       (let loop ([i 0])
                         (if (fx= i length) bytes
                             (begin
                               (store! bytes i (apply proc (values-at i sources)))
                               (loop (fx1+ i))))))))]))
      (define map!/i
        (case-lambda
          [(proc bytes)
           (pcheck ([procedure? proc] [bytevector? bytes])
                   (let ([length (length-of bytes)])
                     (let loop ([i 0])
                       (if (fx= i length) bytes
                           (begin
                             (store! bytes i (proc i (value-at bytes i)))
                             (loop (fx1+ i)))))))]
          [(proc bytes other)
           (pcheck ([procedure? proc] [bytevector? bytes other])
                   (let ([length (source-length bytes other)])
                     (let loop ([i 0])
                       (if (fx= i length) bytes
                           (begin
                             (store! bytes i
                                     (proc i (value-at bytes i) (value-at other i)))
                             (loop (fx1+ i)))))))]
          [(proc bytes second third . rest)
           (let ([bytevectors (cons second (cons third rest))])
             (pcheck ([procedure? proc] [bytevector? bytes]
                      [all-bytevectors? bytevectors])
                     (let* ([sources (cons bytes bytevectors)]
                            [length (apply source-length bytes bytevectors)])
                       (let loop ([i 0])
                         (if (fx= i length) bytes
                             (begin
                               (store! bytes i (apply proc i (values-at i sources)))
                               (loop (fx1+ i))))))))]))
      (define each
        (case-lambda
          [(proc bytes)
           (pcheck ([procedure? proc] [bytevector? bytes])
                   (let ([length (length-of bytes)])
                     (let loop ([i 0])
                       (unless (fx= i length)
                         (proc (value-at bytes i))
                         (loop (fx1+ i))))))]
          [(proc bytes other)
           (pcheck ([procedure? proc] [bytevector? bytes other])
                   (let ([length (source-length bytes other)])
                     (let loop ([i 0])
                       (unless (fx= i length)
                         (proc (value-at bytes i) (value-at other i))
                         (loop (fx1+ i))))))]
          [(proc bytes second third . rest)
           (let ([bytevectors (cons second (cons third rest))])
             (pcheck ([procedure? proc] [bytevector? bytes]
                      [all-bytevectors? bytevectors])
                     (let* ([sources (cons bytes bytevectors)]
                            [length (apply source-length bytes bytevectors)])
                       (let loop ([i 0])
                         (unless (fx= i length)
                           (apply proc (values-at i sources))
                           (loop (fx1+ i)))))))]))
      (define each/i
        (case-lambda
          [(proc bytes)
           (pcheck ([procedure? proc] [bytevector? bytes])
                   (let ([length (length-of bytes)])
                     (let loop ([i 0])
                       (unless (fx= i length)
                         (proc i (value-at bytes i))
                         (loop (fx1+ i))))))]
          [(proc bytes other)
           (pcheck ([procedure? proc] [bytevector? bytes other])
                   (let ([length (source-length bytes other)])
                     (let loop ([i 0])
                       (unless (fx= i length)
                         (proc i (value-at bytes i) (value-at other i))
                         (loop (fx1+ i))))))]
          [(proc bytes second third . rest)
           (let ([bytevectors (cons second (cons third rest))])
             (pcheck ([procedure? proc] [bytevector? bytes]
                      [all-bytevectors? bytevectors])
                     (let* ([sources (cons bytes bytevectors)]
                            [length (apply source-length bytes bytevectors)])
                       (let loop ([i 0])
                         (unless (fx= i length)
                           (apply proc i (values-at i sources))
                           (loop (fx1+ i)))))))]))
      (define slice
        (case-lambda
          [(bytes stop) (slice bytes 0 stop 1)]
          [(bytes start stop) (slice bytes start stop 1)]
          ((bytes start stop step)
           (pcheck ([bytevector? bytes] [fixnum? start stop step])
                   (when (fx= step 0) (errorf who "step cannot be zero"))
                   (let* ([len (length-of bytes)]
                          [s0 (if (fx>= start 0) start (fx+ len start))]
                          [s (cond [(fx< s0 0) 0]
                                   [(fx> s0 len) (fx1- len)]
                                   [else s0])]
                          [e0 (if (fx>= stop 0) stop (fx+ len stop))]
                          [e (cond [(fx<= e0 -1) -1]
                                   [(fx>= e0 len) len]
                                   [else e0])])
                     (if (fx= len 0)
                         (make-result 0)
                         (let* ([count (let count ([i s] [n 0])
                                         (if (if (fx> step 0) (fx>= i e) (fx<= i e))
                                             n
                                             (count (fx+ i step) (fx1+ n))))]
                                [result (make-result count)])
                           (let loop ([i s] [output 0])
                             (if (fx= output count) result
                                 (begin (store! result output (value-at bytes i))
                                        (loop (fx+ i step) (fx1+ output))))))))))))
      (define filter-values
        (lambda (pred bytes)
          (pcheck ([procedure? pred] [bytevector? bytes])
                  (let* ([length (length-of bytes)] [temporary (make-result length)])
                    (let loop ([i 0] [output 0])
                      (if (fx= i length)
                          (let ([result (make-result output)])
                            (bytevector-copy! temporary 0 result 0 (bytevector-length result))
                            result)
                          (let ([value (value-at bytes i)])
                            (if (pred value)
                                (begin (store! temporary output value)
                                       (loop (fx1+ i) (fx1+ output)))
                                (loop (fx1+ i) output)))))))))
      (define partition
        (lambda (pred bytes)
          (pcheck ([procedure? pred] [bytevector? bytes])
                  (let* ([length (length-of bytes)]
                         [yes-temp (make-result length)] [no-temp (make-result length)])
                    (let loop ([i 0] [yes-count 0] [no-count 0])
                      (if (fx= i length)
                          (let ([yes (make-result yes-count)] [no (make-result no-count)])
                            (bytevector-copy! yes-temp 0 yes 0 (bytevector-length yes))
                            (bytevector-copy! no-temp 0 no 0 (bytevector-length no))
                            (values yes no))
                          (let ([value (value-at bytes i)])
                            (if (pred value)
                                (begin (store! yes-temp yes-count value)
                                       (loop (fx1+ i) (fx1+ yes-count) no-count))
                                (begin (store! no-temp no-count value)
                                       (loop (fx1+ i) yes-count (fx1+ no-count)))))))))))
      (define or-values
        (case-lambda
          [(pred bytes)
           (pcheck ([procedure? pred] [bytevector? bytes])
                   (let ([length (length-of bytes)])
                     (let loop ([i 0])
                       (and (fx< i length)
                            (or (pred (value-at bytes i)) (loop (fx1+ i)))))))]
          [(pred bytes other)
           (pcheck ([procedure? pred] [bytevector? bytes other])
                   (let ([length (source-length bytes other)])
                     (let loop ([i 0])
                       (and (fx< i length)
                            (or (pred (value-at bytes i) (value-at other i))
                                (loop (fx1+ i)))))))]
          [(pred bytes second third . rest)
           (let ([bytevectors (cons second (cons third rest))])
             (pcheck ([procedure? pred] [bytevector? bytes]
                      [all-bytevectors? bytevectors])
                     (let* ([sources (cons bytes bytevectors)]
                            [length (apply source-length bytes bytevectors)])
                       (let loop ([i 0])
                         (and (fx< i length)
                              (or (apply pred (values-at i sources))
                                  (loop (fx1+ i))))))))]))
      (define and-values
        (case-lambda
          [(pred bytes)
           (pcheck ([procedure? pred] [bytevector? bytes])
                   (let ([length (length-of bytes)])
                     (let loop ([i 0])
                       (or (fx= i length)
                           (and (pred (value-at bytes i)) (loop (fx1+ i)))))))]
          [(pred bytes other)
           (pcheck ([procedure? pred] [bytevector? bytes other])
                   (let ([length (source-length bytes other)])
                     (let loop ([i 0])
                       (or (fx= i length)
                           (and (pred (value-at bytes i) (value-at other i))
                                (loop (fx1+ i)))))))]
          [(pred bytes second third . rest)
           (let ([bytevectors (cons second (cons third rest))])
             (pcheck ([procedure? pred] [bytevector? bytes]
                      [all-bytevectors? bytevectors])
                     (let* ([sources (cons bytes bytevectors)]
                            [length (apply source-length bytes bytevectors)])
                       (let loop ([i 0])
                         (or (fx= i length)
                             (and (apply pred (values-at i sources))
                                  (loop (fx1+ i))))))))]))
      (define memp
        (lambda (pred bytes)
          (pcheck ([procedure? pred] [bytevector? bytes])
                  (let ([length (length-of bytes)])
                    (let loop ([i 0])
                      (cond [(fx= i length) #f]
                            [(pred (value-at bytes i)) i]
                            [else (loop (fx1+ i))]))))))
      (define member-value (lambda (value bytes) (memp (lambda (item) (equal? value item)) bytes)))
      (define memq-value (lambda (value bytes) (memp (lambda (item) (eq? value item)) bytes)))
      (define memv-value (lambda (value bytes) (memp (lambda (item) (eqv? value item)) bytes)))
      (define fold-left-values
        (case-lambda
          [(proc init bytes)
           (pcheck ([procedure? proc] [bytevector? bytes])
                   (let ([length (length-of bytes)])
                     (let loop ([i 0] [acc init])
                       (if (fx= i length) acc
                           (loop (fx1+ i) (proc acc (value-at bytes i)))))))]
          [(proc init bytes other)
           (pcheck ([procedure? proc] [bytevector? bytes other])
                   (let ([length (source-length bytes other)])
                     (let loop ([i 0] [acc init])
                       (if (fx= i length) acc
                           (loop (fx1+ i)
                                 (proc acc (value-at bytes i) (value-at other i)))))))]
          [(proc init bytes second third . rest)
           (let ([bytevectors (cons second (cons third rest))])
             (pcheck ([procedure? proc] [bytevector? bytes]
                      [all-bytevectors? bytevectors])
                     (let* ([sources (cons bytes bytevectors)]
                            [length (apply source-length bytes bytevectors)])
                       (let loop ([i 0] [acc init])
                         (if (fx= i length) acc
                             (loop (fx1+ i)
                                   (apply proc acc (values-at i sources))))))))]))
      (define fold-right-values
        (case-lambda
          [(proc init bytes)
           (pcheck ([procedure? proc] [bytevector? bytes])
                   (let loop ([i (fx1- (length-of bytes))] [acc init])
                     (if (fx< i 0) acc
                         (loop (fx1- i) (proc (value-at bytes i) acc)))))]
          [(proc init bytes other)
           (pcheck ([procedure? proc] [bytevector? bytes other])
                   (let loop ([i (fx1- (source-length bytes other))] [acc init])
                     (if (fx< i 0) acc
                         (loop (fx1- i)
                               (proc (value-at bytes i) (value-at other i) acc)))))]
          [(proc init bytes second third . rest)
           (let ([bytevectors (cons second (cons third rest))])
             (pcheck ([procedure? proc] [bytevector? bytes]
                      [all-bytevectors? bytevectors])
                     (let ([sources (cons bytes bytevectors)])
                       (let loop ([i (fx1- (apply source-length bytes bytevectors))]
                                  [acc init])
                         (if (fx< i 0) acc
                             (loop (fx1- i)
                                   (apply proc
                                          (append (values-at i sources)
                                                  (list acc)))))))))]))
      (define fold-left/i
        (case-lambda
          [(proc init bytes)
           (pcheck ([procedure? proc] [bytevector? bytes])
                   (let ([length (length-of bytes)])
                     (let loop ([i 0] [acc init])
                       (if (fx= i length) acc
                           (loop (fx1+ i) (proc i acc (value-at bytes i)))))))]
          [(proc init bytes other)
           (pcheck ([procedure? proc] [bytevector? bytes other])
                   (let ([length (source-length bytes other)])
                     (let loop ([i 0] [acc init])
                       (if (fx= i length) acc
                           (loop (fx1+ i)
                                 (proc i acc (value-at bytes i)
                                       (value-at other i)))))))]
          [(proc init bytes second third . rest)
           (let ([bytevectors (cons second (cons third rest))])
             (pcheck ([procedure? proc] [bytevector? bytes]
                      [all-bytevectors? bytevectors])
                     (let* ([sources (cons bytes bytevectors)]
                            [length (apply source-length bytes bytevectors)])
                       (let loop ([i 0] [acc init])
                         (if (fx= i length) acc
                             (loop (fx1+ i)
                                   (apply proc i acc (values-at i sources))))))))]))
      (define fold-right/i
        (case-lambda
          [(proc init bytes)
           (pcheck ([procedure? proc] [bytevector? bytes])
                   (let loop ([i (fx1- (length-of bytes))] [acc init])
                     (if (fx< i 0) acc
                         (loop (fx1- i) (proc i (value-at bytes i) acc)))))]
          [(proc init bytes other)
           (pcheck ([procedure? proc] [bytevector? bytes other])
                   (let loop ([i (fx1- (source-length bytes other))] [acc init])
                     (if (fx< i 0) acc
                         (loop (fx1- i)
                               (proc i (value-at bytes i) (value-at other i) acc)))))]
          [(proc init bytes second third . rest)
           (let ([bytevectors (cons second (cons third rest))])
             (pcheck ([procedure? proc] [bytevector? bytes]
                      [all-bytevectors? bytevectors])
                     (let ([sources (cons bytes bytevectors)])
                       (let loop ([i (fx1- (apply source-length bytes bytevectors))]
                                  [acc init])
                         (if (fx< i 0) acc
                             (loop (fx1- i)
                                   (apply proc i
                                          (append (values-at i sources)
                                                  (list acc)))))))))]))
      (define scan-left-ex
        (case-lambda
          [(proc init bytes)
           (pcheck ([procedure? proc] [bytevector? bytes])
                   (let* ([length (length-of bytes)] [result (make-result length)])
                     (let loop ([i 0] [acc init])
                       (if (fx= i length) result
                           (begin (store! result i acc)
                                  (loop (fx1+ i)
                                        (proc acc (value-at bytes i))))))))]
          [(proc init bytes other)
           (pcheck ([procedure? proc] [bytevector? bytes other])
                   (let* ([length (source-length bytes other)]
                          [result (make-result length)])
                     (let loop ([i 0] [acc init])
                       (if (fx= i length) result
                           (begin
                             (store! result i acc)
                             (loop (fx1+ i)
                                   (proc acc (value-at bytes i)
                                         (value-at other i))))))))]
          [(proc init bytes second third . rest)
           (let ([bytevectors (cons second (cons third rest))])
             (pcheck ([procedure? proc] [bytevector? bytes]
                      [all-bytevectors? bytevectors])
                     (let* ([sources (cons bytes bytevectors)]
                            [length (apply source-length bytes bytevectors)]
                            [result (make-result length)])
                       (let loop ([i 0] [acc init])
                         (if (fx= i length) result
                             (begin
                               (store! result i acc)
                               (loop (fx1+ i)
                                     (apply proc acc (values-at i sources)))))))))]))
      (define scan-left-in
        (case-lambda
          [(proc init bytes)
           (pcheck ([procedure? proc] [bytevector? bytes])
                   (let* ([length (length-of bytes)] [result (make-result length)])
                     (let loop ([i 0] [acc init])
                       (if (fx= i length) result
                           (let ([next (proc acc (value-at bytes i))])
                             (store! result i next)
                             (loop (fx1+ i) next))))))]
          [(proc init bytes other)
           (pcheck ([procedure? proc] [bytevector? bytes other])
                   (let* ([length (source-length bytes other)]
                          [result (make-result length)])
                     (let loop ([i 0] [acc init])
                       (if (fx= i length) result
                           (let ([next (proc acc (value-at bytes i)
                                             (value-at other i))])
                             (store! result i next)
                             (loop (fx1+ i) next))))))]
          [(proc init bytes second third . rest)
           (let ([bytevectors (cons second (cons third rest))])
             (pcheck ([procedure? proc] [bytevector? bytes]
                      [all-bytevectors? bytevectors])
                     (let* ([sources (cons bytes bytevectors)]
                            [length (apply source-length bytes bytevectors)]
                            [result (make-result length)])
                       (let loop ([i 0] [acc init])
                         (if (fx= i length) result
                             (let ([next (apply proc acc (values-at i sources))])
                               (store! result i next)
                               (loop (fx1+ i) next)))))))]))
      (define scan-right-ex
        (case-lambda
          [(proc init bytes)
           (pcheck ([procedure? proc] [bytevector? bytes])
                   (let* ([length (length-of bytes)] [result (make-result length)])
                     (when (fx> length 0) (store! result 0 init))
                     (let loop ([i (fx1- length)] [output 1] [acc init])
                       (if (fx<= i 0) result
                           (let ([next (proc (value-at bytes i) acc)])
                             (store! result output next)
                             (loop (fx1- i) (fx1+ output) next))))))]
          [(proc init bytes other)
           (pcheck ([procedure? proc] [bytevector? bytes other])
                   (let* ([length (source-length bytes other)]
                          [result (make-result length)])
                     (when (fx> length 0) (store! result 0 init))
                     (let loop ([i (fx1- length)] [output 1] [acc init])
                       (if (fx<= i 0) result
                           (let ([next (proc (value-at bytes i)
                                             (value-at other i) acc)])
                             (store! result output next)
                             (loop (fx1- i) (fx1+ output) next))))))]
          [(proc init bytes second third . rest)
           (let ([bytevectors (cons second (cons third rest))])
             (pcheck ([procedure? proc] [bytevector? bytes]
                      [all-bytevectors? bytevectors])
                     (let* ([sources (cons bytes bytevectors)]
                            [length (apply source-length bytes bytevectors)]
                            [result (make-result length)])
                       (when (fx> length 0) (store! result 0 init))
                       (let loop ([i (fx1- length)] [output 1] [acc init])
                         (if (fx<= i 0) result
                             (let ([next
                                    (apply proc
                                           (append (values-at i sources)
                                                   (list acc)))])
                               (store! result output next)
                               (loop (fx1- i) (fx1+ output) next)))))))]))
      (define scan-right-in
        (case-lambda
          [(proc init bytes)
           (pcheck ([procedure? proc] [bytevector? bytes])
                   (let* ([length (length-of bytes)] [result (make-result length)])
                     (let loop ([i (fx1- length)] [output 0] [acc init])
                       (if (fx< i 0) result
                           (let ([next (proc (value-at bytes i) acc)])
                             (store! result output next)
                             (loop (fx1- i) (fx1+ output) next))))))]
          [(proc init bytes other)
           (pcheck ([procedure? proc] [bytevector? bytes other])
                   (let* ([length (source-length bytes other)]
                          [result (make-result length)])
                     (let loop ([i (fx1- length)] [output 0] [acc init])
                       (if (fx< i 0) result
                           (let ([next (proc (value-at bytes i)
                                             (value-at other i) acc)])
                             (store! result output next)
                             (loop (fx1- i) (fx1+ output) next))))))]
          [(proc init bytes second third . rest)
           (let ([bytevectors (cons second (cons third rest))])
             (pcheck ([procedure? proc] [bytevector? bytes]
                      [all-bytevectors? bytevectors])
                     (let* ([sources (cons bytes bytevectors)]
                            [length (apply source-length bytes bytevectors)]
                            [result (make-result length)])
                       (let loop ([i (fx1- length)] [output 0] [acc init])
                         (if (fx< i 0) result
                             (let ([next
                                    (apply proc
                                           (append (values-at i sources)
                                                   (list acc)))])
                               (store! result output next)
                               (loop (fx1- i) (fx1+ output) next)))))))]))
      (define reverse-values
        (lambda (bytes)
          (let* ([length (length-of bytes)] [result (make-result length)])
            (let loop ([i 0])
              (if (fx= i length) result
                  (begin (store! result i (value-at bytes (fx- length i 1)))
                         (loop (fx1+ i))))))))
      (define reverse-values! (lambda (bytes) (replace! bytes (reverse-values bytes))))
      (define zip
        (lambda (bytes . rest)
          (pcheck ([bytevector? bytes] [all-bytevectors? rest])
                  (when (null? rest) (errorf who "zip requires at least two bytevectors"))
                  (let* ([sources (cons bytes rest)]
                         [length (apply source-length bytes rest)]
                         [result (make-vector length)])
                    (let loop ([i 0])
                      (if (fx= i length) result
                          (begin (vector-set! result i (values-at i sources))
                                 (loop (fx1+ i)))))))))
      (define zipv
        (lambda (bytes . rest)
          (pcheck ([bytevector? bytes] [all-bytevectors? rest])
                  (when (null? rest) (errorf who "zipv requires at least two bytevectors"))
                  (let* ([sources (cons bytes rest)]
                         [length (apply source-length bytes rest)]
                         [result (make-vector length)])
                    (let loop ([i 0])
                      (if (fx= i length) result
                          (let* ([values (values-at i sources)]
                                 [row (make-result (length values))])
                            (let fill ([j 0] [values values])
                              (unless (null? values)
                                (store! row j (car values))
                                (fill (fx1+ j) (cdr values))))
                            (vector-set! result i row)
                            (loop (fx1+ i)))))))))
      (define shuffle
        (lambda (bytes)
          (let ([result (make-bytevector (bytevector-length bytes) 0)])
            (bytevector-copy! bytes 0 result 0 (bytevector-length bytes))
            (let loop ([i (fx1- (length-of result))])
              (when (fx> i 0)
                (let* ([j (random (fx1+ i))] [left (value-at result i)])
                  (store! result i (value-at result j))
                  (store! result j left)
                  (loop (fx1- i)))))
            result)))
      (define shuffle! (lambda (bytes) (replace! bytes (shuffle bytes))))
      (define sort-direct
        (case-lambda
          [(less? bytes)
           (pcheck ([procedure? less?] [bytevector? bytes])
                   (let ([result (make-bytevector (bytevector-length bytes) 0)])
                     (bytevector-copy! bytes 0 result 0 (bytevector-length bytes))
                     ($bvector-merge-sort! less? result 0
                                            ($bvector-length who result width)
                                            width ref set)
                     result))]
          [(less? bytes stop)
           (sort-direct less? bytes 0 stop)]
          [(less? bytes start stop)
           (pcheck ([procedure? less?] [bytevector? bytes] [natural? start stop])
                   (let ([length ($bvector-length who bytes width)])
                     (when (fx> stop length)
                       (errorf who "stop index ~a out of bound ~a" stop length))
                     (when (fx> start stop)
                       (errorf who "start index ~a greater than stop index ~a" start stop))
                     (let ([result (make-bytevector (fx* width (fx- stop start)) 0)])
                       (let loop ([i start])
                         (unless (fx= i stop)
                           (set result (fx* (fx- i start) width)
                                (ref bytes (fx* i width)))
                           (loop (fx1+ i))))
                       ($bvector-merge-sort! less? result 0 (fx- stop start)
                                              width ref set)
                       result)))]))
      (define sort-direct!
        (case-lambda
          [(less? bytes) ($bvector-merge-sort! less? bytes 0
                                                ($bvector-length who bytes width)
                                                width ref set)]
          [(less? bytes stop) (sort-direct! less? bytes 0 stop)]
          [(less? bytes start stop)
           (pcheck ([procedure? less?] [bytevector? bytes] [natural? start stop])
                   (let ([length ($bvector-length who bytes width)])
                     (when (fx> stop length)
                       (errorf who "stop index ~a out of bound ~a" stop length))
                     (when (fx> start stop)
                       (errorf who "start index ~a greater than stop index ~a" start stop))
                     ($bvector-merge-sort! less? bytes start stop width ref set)
                     bytes))]))
      (define sorted?
        (case-lambda
          [(less? bytes) (sorted? less? bytes 0 (length-of bytes))]
          [(less? bytes stop) (sorted? less? bytes 0 stop)]
          [(less? bytes start stop)
           (pcheck ([procedure? less?] [bytevector? bytes] [natural? start stop])
                   (let ([length (length-of bytes)])
                     (when (fx> stop length)
                       (errorf who "stop index ~a out of bound ~a" stop length))
                     (when (fx> start stop)
                       (errorf who "start index ~a greater than stop index ~a" start stop))
                     (let loop ([i (fx1+ start)])
                       (or (fx>= i stop)
                           (and (not (less? (value-at bytes i)
                                            (value-at bytes (fx1- i))))
                                (loop (fx1+ i)))))))]))
      (define copy
        (lambda (bytes)
          (pcheck ([bytevector? bytes])
                  (length-of bytes)
                  (let ([result (make-bytevector (bytevector-length bytes) 0)])
                    (bytevector-copy! bytes 0 result 0 (bytevector-length bytes))
                    result))))
      (define copy!
        (lambda (src src-start target target-start count)
          (pcheck ([bytevector? src target] [natural? src-start target-start count])
                  (let ([source-length (length-of src)])
                    (when (fx> (fx+ src-start count) source-length)
                      (errorf who "source range is too large"))
                    (when (fx> (fx+ target-start count) (length-of target))
                      (errorf who "target range is too large"))
                    (bytevector-copy! src (fx* src-start width)
                                      target (fx* target-start width)
                                      (fx* count width))
                    target))))
      (define sum
        (lambda (bytes)
          (let ([length (length-of bytes)])
            (let loop ([i 0] [answer zero])
              (if (fx= i length) answer
                  (loop (fx1+ i) (+ answer (value-at bytes i))))))))
      (define product
        (lambda (bytes)
          (let ([length (length-of bytes)])
            (let loop ([i 0] [answer one])
              (if (fx= i length) answer
                  (loop (fx1+ i) (* answer (value-at bytes i))))))))
      (define extreme
        (lambda (better? bytes)
          (let ([length (length-of bytes)])
            (and (fx> length 0)
                 (let loop ([i 1] [best (value-at bytes 0)])
                   (if (fx= i length) best
                       (let ([value (value-at bytes i)])
                         (loop (fx1+ i) (if (better? value best) value best)))))))))
      (define maximum (lambda (bytes) (extreme > bytes)))
      (define minimum (lambda (bytes) (extreme < bytes)))
      (define average
        (lambda (bytes)
          (let ([length ($bvector-length who bytes width)])
            (if (fx= length 0) #f (/ (sum bytes) length)))))
      (define nums
        (case-lambda
          [(stop) (nums zero stop one)] [(start stop) (nums start stop one)]
          [(start stop step)
           (pcheck ([number? start stop step])
                   (unless (or (and (< start stop) (> step 0))
                               (and (> start stop) (< step 0))
                               (= start stop))
                     (errorf who "invalid range: ~a, ~a, ~a" start stop step))
                   (let ([length (let count ([value start] [n 0])
                                   (if (if (> step 0) (>= value stop) (<= value stop))
                                       n
                                       (count (+ value step) (fx1+ n))))])
                     (let ([result (make-result length)])
                       (let loop ([value start] [i 0])
                         (if (fx= i length) result
                             (begin (store! result i value)
                                    (loop (+ value step) (fx1+ i))))))))]))
      (vector map-values map/i map! map!/i each each/i slice filter-values partition
              or-values and-values or-values and-values memp member-value memq-value memv-value
              fold-left-values fold-right-values fold-left/i fold-right/i
              scan-left-ex scan-left-in scan-right-ex scan-right-in reverse-values reverse-values!
              zip zipv shuffle shuffle! sort-direct sort-direct! sorted? copy copy!
              sum product extreme maximum minimum average nums)))

  (define bvector-u8-value? (lambda (value) (and (and (integer? value) (exact? value)) (<= 0 value 255))))
  (define bvector-s8-value? (lambda (value) (and (and (integer? value) (exact? value)) (<= -128 value 127))))
  (define bvector-u16-value? (lambda (value) (and (and (integer? value) (exact? value)) (<= 0 value 65535))))
  (define bvector-s16-value? (lambda (value) (and (and (integer? value) (exact? value)) (<= -32768 value 32767))))
  (define bvector-u24-value? (lambda (value) (and (and (integer? value) (exact? value)) (<= 0 value 16777215))))
  (define bvector-s24-value? (lambda (value) (and (and (integer? value) (exact? value)) (<= -8388608 value 8388607))))
  (define bvector-u32-value? (lambda (value) (and (and (integer? value) (exact? value)) (<= 0 value 4294967295))))
  (define bvector-s32-value? (lambda (value) (and (and (integer? value) (exact? value)) (<= -2147483648 value 2147483647))))
  (define bvector-u40-value? (lambda (value) (and (and (integer? value) (exact? value)) (<= 0 value 1099511627775))))
  (define bvector-s40-value? (lambda (value) (and (and (integer? value) (exact? value)) (<= -549755813888 value 549755813887))))
  (define bvector-u48-value? (lambda (value) (and (and (integer? value) (exact? value)) (<= 0 value 281474976710655))))
  (define bvector-s48-value? (lambda (value) (and (and (integer? value) (exact? value)) (<= -140737488355328 value 140737488355327))))
  (define bvector-u56-value? (lambda (value) (and (and (integer? value) (exact? value)) (<= 0 value 72057594037927935))))
  (define bvector-s56-value? (lambda (value) (and (and (integer? value) (exact? value)) (<= -36028797018963968 value 36028797018963967))))
  (define bvector-u64-value? (lambda (value) (and (and (integer? value) (exact? value)) (<= 0 value 18446744073709551615))))
  (define bvector-s64-value? (lambda (value) (and (and (integer? value) (exact? value)) (<= -9223372036854775808 value 9223372036854775807))))

  #|macro:define-bvector-procedure
  Define the fixed-width bytevector operation family for descriptor `width`.
  The `width-size` value is the number of bytes per logical value. The `ref` and `set`
  procedures have signatures `(bytevector byte-offset) -> value` and
  `(bytevector byte-offset value) -> unspecified`. Predicate `value?` has signature
  `(value) -> boolean` and validates produced values.

  Generated map and iteration procedures take a procedure and one or more equal-length
  bytevectors. A non-indexed procedure has signature `(value ...) -> value`; an indexed
  procedure has signature `(index value ...) -> value`. Iteration return values are
  unspecified. Maps return a new bytevector, except mutating maps store results directly
  in and return the first bytevector. Predicate traversals use `(value ...) -> any` and
  short-circuit.

  Left folds and scans use `(accumulator value ...) -> accumulator`. Right folds and scans
  use `(value ... accumulator) -> accumulator`. Indexed folds receive `index` first. Folds
  return the final accumulator, and scans return a bytevector of intermediate accumulators.
  |#
  (define-syntax define-bvector-procedure
    (lambda (stx)
      (syntax-case stx ()
        [(_ width width-size ref set value?)
         (let ([name (symbol->string (syntax->datum #'width))])
           (with-syntax
               ([operations ($construct-name #'width "$bvector-" name "-operations")]
                     [n0 ($construct-name #'width "bvmap-" name)]
                     [n1 ($construct-name #'width "bvmap/i-" name)]
                     [n2 ($construct-name #'width "bvmap!-" name)]
                     [n3 ($construct-name #'width "bvmap!/i-" name)]
                     [n4 ($construct-name #'width "bvfor-each-" name)]
                     [n5 ($construct-name #'width "bvfor-each/i-" name)]
                     [n6 ($construct-name #'width "bvslice-" name)]
                     [n7 ($construct-name #'width "bvfilter-" name)]
                     [n8 ($construct-name #'width "bvpartition-" name)]
                     [n9 ($construct-name #'width "bvormap-" name)]
                     [n10 ($construct-name #'width "bvandmap-" name)]
                     [n11 ($construct-name #'width "bvexists-" name)]
                     [n12 ($construct-name #'width "bvfor-all-" name)]
                     [n13 ($construct-name #'width "bvmemp-" name)]
                     [n14 ($construct-name #'width "bvmember-" name)]
                     [n15 ($construct-name #'width "bvmemq-" name)]
                     [n16 ($construct-name #'width "bvmemv-" name)]
                     [n17 ($construct-name #'width "bvfold-left-" name)]
                     [n18 ($construct-name #'width "bvfold-right-" name)]
                     [n19 ($construct-name #'width "bvfold-left/i-" name)]
                     [n20 ($construct-name #'width "bvfold-right/i-" name)]
                     [n21 ($construct-name #'width "bvscan-left-ex-" name)]
                     [n22 ($construct-name #'width "bvscan-left-in-" name)]
                     [n23 ($construct-name #'width "bvscan-right-ex-" name)]
                     [n24 ($construct-name #'width "bvscan-right-in-" name)]
                     [n25 ($construct-name #'width "bvreverse-" name)]
                     [n26 ($construct-name #'width "bvreverse!-" name)]
                     [n27 ($construct-name #'width "bvzip-" name)]
                     [n28 ($construct-name #'width "bvzipv-" name)]
                     [n29 ($construct-name #'width "bvshuffle-" name)]
                     [n30 ($construct-name #'width "bvshuffle!-" name)]
                     [n31 ($construct-name #'width "bvsort-" name)]
                     [n32 ($construct-name #'width "bvsort!-" name)]
                     [n33 ($construct-name #'width "bvsorted?-" name)]
                     [n34 ($construct-name #'width "bvcopy-" name)]
                     [n35 ($construct-name #'width "bvcopy!-" name)]
                     [n36 ($construct-name #'width "bvsum-" name)]
                     [n37 ($construct-name #'width "bvproduct-" name)]
                     [n38 ($construct-name #'width "bvextreme-" name)]
                     [n39 ($construct-name #'width "bvmax-" name)]
                     [n40 ($construct-name #'width "bvmin-" name)]
                     [n41 ($construct-name #'width "bvavg-" name)]
                     [n42 ($construct-name #'width "bvnums-" name)]
                     [n43 ($construct-name #'width "bvector-" name "-map/i")]
                     [n44 ($construct-name #'width "bvector-" name "-map!")]
                     [n45 ($construct-name #'width "bvector-" name "-map!/i")]
                     [n46 ($construct-name #'width "bvector-" name "-for-each/i")])
             #'(begin
                (define operations
                  ($make-bvector-operations 'width width-size ref set value?))
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
                (define n43 (vector-ref operations 1))
                (define n44 (vector-ref operations 2))
                (define n45 (vector-ref operations 3))
                (define n46 (vector-ref operations 5)))))])))


  (define-bvector-procedure u8 1
    (lambda (bytes offset) (bytevector-u8-ref bytes offset))
    (lambda (bytes offset value) (bytevector-u8-set! bytes offset value)) bvector-u8-value?)
  (define-bvector-procedure U8 1
    (lambda (bytes offset) (bytevector-u8-ref bytes offset))
    (lambda (bytes offset value) (bytevector-u8-set! bytes offset value)) bvector-u8-value?)
  (define-bvector-procedure s8 1
    (lambda (bytes offset) (bytevector-s8-ref bytes offset))
    (lambda (bytes offset value) (bytevector-s8-set! bytes offset value)) bvector-s8-value?)
  (define-bvector-procedure S8 1
    (lambda (bytes offset) (bytevector-s8-ref bytes offset))
    (lambda (bytes offset value) (bytevector-s8-set! bytes offset value)) bvector-s8-value?)
  (define-bvector-procedure u16 2
    (lambda (bytes offset) (bytevector-u16-ref bytes offset (endianness little)))
    (lambda (bytes offset value) (bytevector-u16-set! bytes offset value (endianness little))) bvector-u16-value?)
  (define-bvector-procedure U16 2
    (lambda (bytes offset) (bytevector-u16-ref bytes offset (endianness big)))
    (lambda (bytes offset value) (bytevector-u16-set! bytes offset value (endianness big))) bvector-u16-value?)
  (define-bvector-procedure s16 2
    (lambda (bytes offset) (bytevector-s16-ref bytes offset (endianness little)))
    (lambda (bytes offset value) (bytevector-s16-set! bytes offset value (endianness little))) bvector-s16-value?)
  (define-bvector-procedure S16 2
    (lambda (bytes offset) (bytevector-s16-ref bytes offset (endianness big)))
    (lambda (bytes offset value) (bytevector-s16-set! bytes offset value (endianness big))) bvector-s16-value?)
  (define-bvector-procedure u24 3
    (lambda (bytes offset) (bytevector-u24-ref bytes offset (endianness little)))
    (lambda (bytes offset value) (bytevector-u24-set! bytes offset value (endianness little))) bvector-u24-value?)
  (define-bvector-procedure U24 3
    (lambda (bytes offset) (bytevector-u24-ref bytes offset (endianness big)))
    (lambda (bytes offset value) (bytevector-u24-set! bytes offset value (endianness big))) bvector-u24-value?)
  (define-bvector-procedure s24 3
    (lambda (bytes offset) (bytevector-s24-ref bytes offset (endianness little)))
    (lambda (bytes offset value) (bytevector-s24-set! bytes offset value (endianness little))) bvector-s24-value?)
  (define-bvector-procedure S24 3
    (lambda (bytes offset) (bytevector-s24-ref bytes offset (endianness big)))
    (lambda (bytes offset value) (bytevector-s24-set! bytes offset value (endianness big))) bvector-s24-value?)
  (define-bvector-procedure u32 4
    (lambda (bytes offset) (bytevector-u32-ref bytes offset (endianness little)))
    (lambda (bytes offset value) (bytevector-u32-set! bytes offset value (endianness little))) bvector-u32-value?)
  (define-bvector-procedure U32 4
    (lambda (bytes offset) (bytevector-u32-ref bytes offset (endianness big)))
    (lambda (bytes offset value) (bytevector-u32-set! bytes offset value (endianness big))) bvector-u32-value?)
  (define-bvector-procedure s32 4
    (lambda (bytes offset) (bytevector-s32-ref bytes offset (endianness little)))
    (lambda (bytes offset value) (bytevector-s32-set! bytes offset value (endianness little))) bvector-s32-value?)
  (define-bvector-procedure S32 4
    (lambda (bytes offset) (bytevector-s32-ref bytes offset (endianness big)))
    (lambda (bytes offset value) (bytevector-s32-set! bytes offset value (endianness big))) bvector-s32-value?)
  (define-bvector-procedure u40 5
    (lambda (bytes offset) (bytevector-u40-ref bytes offset (endianness little)))
    (lambda (bytes offset value) (bytevector-u40-set! bytes offset value (endianness little))) bvector-u40-value?)
  (define-bvector-procedure U40 5
    (lambda (bytes offset) (bytevector-u40-ref bytes offset (endianness big)))
    (lambda (bytes offset value) (bytevector-u40-set! bytes offset value (endianness big))) bvector-u40-value?)
  (define-bvector-procedure s40 5
    (lambda (bytes offset) (bytevector-s40-ref bytes offset (endianness little)))
    (lambda (bytes offset value) (bytevector-s40-set! bytes offset value (endianness little))) bvector-s40-value?)
  (define-bvector-procedure S40 5
    (lambda (bytes offset) (bytevector-s40-ref bytes offset (endianness big)))
    (lambda (bytes offset value) (bytevector-s40-set! bytes offset value (endianness big))) bvector-s40-value?)
  (define-bvector-procedure u48 6
    (lambda (bytes offset) (bytevector-u48-ref bytes offset (endianness little)))
    (lambda (bytes offset value) (bytevector-u48-set! bytes offset value (endianness little))) bvector-u48-value?)
  (define-bvector-procedure U48 6
    (lambda (bytes offset) (bytevector-u48-ref bytes offset (endianness big)))
    (lambda (bytes offset value) (bytevector-u48-set! bytes offset value (endianness big))) bvector-u48-value?)
  (define-bvector-procedure s48 6
    (lambda (bytes offset) (bytevector-s48-ref bytes offset (endianness little)))
    (lambda (bytes offset value) (bytevector-s48-set! bytes offset value (endianness little))) bvector-s48-value?)
  (define-bvector-procedure S48 6
    (lambda (bytes offset) (bytevector-s48-ref bytes offset (endianness big)))
    (lambda (bytes offset value) (bytevector-s48-set! bytes offset value (endianness big))) bvector-s48-value?)
  (define-bvector-procedure u56 7
    (lambda (bytes offset) (bytevector-u56-ref bytes offset (endianness little)))
    (lambda (bytes offset value) (bytevector-u56-set! bytes offset value (endianness little))) bvector-u56-value?)
  (define-bvector-procedure U56 7
    (lambda (bytes offset) (bytevector-u56-ref bytes offset (endianness big)))
    (lambda (bytes offset value) (bytevector-u56-set! bytes offset value (endianness big))) bvector-u56-value?)
  (define-bvector-procedure s56 7
    (lambda (bytes offset) (bytevector-s56-ref bytes offset (endianness little)))
    (lambda (bytes offset value) (bytevector-s56-set! bytes offset value (endianness little))) bvector-s56-value?)
  (define-bvector-procedure S56 7
    (lambda (bytes offset) (bytevector-s56-ref bytes offset (endianness big)))
    (lambda (bytes offset value) (bytevector-s56-set! bytes offset value (endianness big))) bvector-s56-value?)
  (define-bvector-procedure u64 8
    (lambda (bytes offset) (bytevector-u64-ref bytes offset (endianness little)))
    (lambda (bytes offset value) (bytevector-u64-set! bytes offset value (endianness little))) bvector-u64-value?)
  (define-bvector-procedure U64 8
    (lambda (bytes offset) (bytevector-u64-ref bytes offset (endianness big)))
    (lambda (bytes offset value) (bytevector-u64-set! bytes offset value (endianness big))) bvector-u64-value?)
  (define-bvector-procedure s64 8
    (lambda (bytes offset) (bytevector-s64-ref bytes offset (endianness little)))
    (lambda (bytes offset value) (bytevector-s64-set! bytes offset value (endianness little))) bvector-s64-value?)
  (define-bvector-procedure S64 8
    (lambda (bytes offset) (bytevector-s64-ref bytes offset (endianness big)))
    (lambda (bytes offset value) (bytevector-s64-set! bytes offset value (endianness big))) bvector-s64-value?)
  (define-bvector-procedure fp32 4
    (lambda (bytes offset) (bytevector-ieee-single-ref bytes offset (endianness little)))
    (lambda (bytes offset value) (bytevector-ieee-single-set! bytes offset value (endianness little))) flonum?)
  (define-bvector-procedure FP32 4
    (lambda (bytes offset) (bytevector-ieee-single-ref bytes offset (endianness big)))
    (lambda (bytes offset value) (bytevector-ieee-single-set! bytes offset value (endianness big))) flonum?)
  (define-bvector-procedure fp64 8
    (lambda (bytes offset) (bytevector-ieee-double-ref bytes offset (endianness little)))
    (lambda (bytes offset value) (bytevector-ieee-double-set! bytes offset value (endianness little))) flonum?)
  (define-bvector-procedure FP64 8
    (lambda (bytes offset) (bytevector-ieee-double-ref bytes offset (endianness big)))
    (lambda (bytes offset value) (bytevector-ieee-double-set! bytes offset value (endianness big))) flonum?)

  )
