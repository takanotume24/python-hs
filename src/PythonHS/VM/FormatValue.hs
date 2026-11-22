module PythonHS.VM.FormatValue (formatValue) where

import Data.List (intercalate)
import Numeric (showFFloat)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Evaluator.ValueToOutput (valueToOutput)
import PythonHS.VM.FormatSpec (FormatSpec (..))
import PythonHS.VM.FormatValueConfig (FormatValueConfig (..))
import PythonHS.VM.ParseFormatSpec (parseFormatSpec)

-- | Apply a Python format specifier to a value.
formatValue :: FormatValueConfig -> Either String String
formatValue config =
  case parseFormatSpec formatSpec of
    Left badSpec ->
      Left ("Value error: unsupported format spec " ++ badSpec ++ " at " ++ showPos pos)
    Right spec -> applySpec spec
  where
    value = formatValueValue config
    formatSpec = formatValueSpec config
    pos = formatValuePos config

    applySpec spec =
      case formatSpecType spec of
        Just 'd' -> applyInt spec
        Just 'f' -> applyFloat spec
        Just 's' -> applyString spec (valueToOutput value)
        _ ->
          case value of
            IntValue {} -> applyInt spec
            FloatValue {} -> applyFloatGeneral spec
            _ -> applyString spec (valueToOutput value)

    applyInt spec =
      case value of
        IntValue {intValue = n} ->
          Right
            ( padNumber
                spec
                (signPrefix spec (fromIntegral n))
                (groupThousands (formatSpecThousands spec) (show (abs n)))
            )
        _ -> Left ("Type error: d format expects int at " ++ showPos pos)

    applyFloat spec =
      case numericValue value of
        Just number ->
          let precision = maybe 6 id (formatSpecPrecision spec)
              body = showFFloat (Just precision) (abs number) ""
           in Right
                ( padNumber
                    spec
                    (signPrefix spec number)
                    (groupThousands (formatSpecThousands spec) body)
                )
        Nothing -> Left ("Type error: f format expects number at " ++ showPos pos)

    applyFloatGeneral spec =
      let text = valueToOutput value
          (textSign, body) =
            case text of
              '-' : rest -> ("-", rest)
              _ -> ("", text)
          sign =
            if textSign == "-"
              then "-"
              else signPrefix spec 0
       in Right
            ( padNumber
                spec
                sign
                (groupThousands (formatSpecThousands spec) body)
            )

    applyString spec text =
      Right (padText (effectiveAlign spec '<') (fillChar spec) (maybe 0 id (formatSpecWidth spec)) text)

    padNumber spec sign body =
      let fullText = sign ++ body
          width = maybe 0 id (formatSpecWidth spec)
          fill = fillChar spec
          align
            | formatSpecAlign spec == Just '=' = '='
            | formatSpecZeroPad spec = '='
            | otherwise = effectiveAlign spec '>'
       in case align of
            '=' ->
              sign
                ++ replicate (max 0 (width - length fullText)) fill
                ++ body
            _ -> padText align fill (maybe 0 id (formatSpecWidth spec)) fullText

    effectiveAlign spec defaultAlign =
      case formatSpecAlign spec of
        Just align
          | align /= '=' -> align
        _ -> defaultAlign

    fillChar spec =
      case formatSpecFill spec of
        Just character -> character
        Nothing
          | formatSpecZeroPad spec -> '0'
          | otherwise -> ' '

    padText align fill width text =
      let padding = max 0 (width - length text)
       in case align of
            '<' -> text ++ replicate padding fill
            '^' ->
              let leftPadding = padding `div` 2
               in replicate leftPadding fill ++ text ++ replicate (padding - leftPadding) fill
            _ -> replicate padding fill ++ text

    signPrefix :: FormatSpec -> Double -> String
    signPrefix spec number
      | number < 0 = "-"
      | formatSpecSign spec == Just '+' = "+"
      | formatSpecSign spec == Just ' ' = " "
      | otherwise = ""

    groupThousands False text = text
    groupThousands True text =
      case break (== '.') text of
        (integerPart, fractionPart) -> groupDigits integerPart ++ fractionPart

    groupDigits digits =
      reverse (intercalate "," (chunksOf 3 (reverse digits)))

    chunksOf _ [] = []
    chunksOf size characters =
      let (headPart, tailPart) = splitAt size characters
       in headPart : chunksOf size tailPart

    numericValue formattedValue =
      case formattedValue of
        IntValue {intValue = n} -> Just (fromIntegral n)
        FloatValue {floatValue = n} -> Just n
        _ -> Nothing
