module Printf

data Format = Number Format
            | Str Format
            | Lit String Format
            | End

||| A type level function used to construct the argument chain based
||| based on the format of the input string.
PrintfType : Format -> Type
PrintfType (Number fmt) = (i : Int) -> PrintfType fmt -- A number
                                    -- input is expected from user.
                                    -- Capture it in the printf signature.
PrintfType (Str fmt) = (s: String) -> PrintfType fmt -- A String input
                                   -- is expected from user. Capture
                                   -- it in the type
PrintfType (Lit x fmt) = PrintfType fmt -- No inputs expected from user
PrintfType End = String -- Return type of printf is always a String

||| Takes a format and an accumulator string to generate the final
||| output string.
printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number x) acc = \i => printfFmt x (acc ++ show i)
printfFmt (Str x) acc = \s => printfFmt x (acc ++ s)
printfFmt (Lit x y) acc = printfFmt y (acc ++ x)
printfFmt End acc = acc

||| toFormat function takes a list of characters and returns Format
toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = Number (toFormat chars)
toFormat ('%' :: 's' :: chars) = Str (toFormat chars)
toFormat ('%' :: chars) = Lit "%" (toFormat chars) -- Not sure why
                                  -- this has to be a separate case.
toFormat (c :: chars) = case toFormat chars of
                             (Lit x y) => Lit (strCons c x) y
                             fmt => Lit (singleton c) fmt

printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""
