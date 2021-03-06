module SplitList

data SplitList : List a -> Type where
     SplitNil  : SplitList []
     SplitOne  : SplitList  [x]
     SplitPair : (lefts : List a) -> (rights : List a) -> SplitList (lefts ++ rights)

total
splitListHelper : List a -> (input : List a) -> SplitList input
splitListHelper _ [] = SplitNil
splitListHelper _ [x] = SplitOne
splitListHelper (_ :: _ :: counter) (item :: items) = case splitListHelper counter items of
                                                           SplitNil => SplitOne
                                                           SplitOne {x} => SplitPair [item] [x]
                                                           (SplitPair lefts rights) => SplitPair (item :: lefts) rights
splitListHelper [] items = SplitPair [] items

splitList : (input : List a) -> SplitList input
splitList input = splitListHelper input input

mergeSort : Ord a => List a -> List a
mergeSort xs with (splitList xs)
  mergeSort [] | SplitNil = []
  mergeSort [x] | SplitOne = [x]
  mergeSort (lefts ++ rights) | (SplitPair lefts rights) = merge (mergeSort lefts) (mergeSort rights)
