module PythonHS.CPython.ParseAllowlistEntry (parseAllowlistEntry) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import PythonHS.CPython.AllowlistEntry (AllowlistEntry (..))

-- | Parse an allowlist entry such as @path@, @path::Class@, or
-- @path::Class::method@.
parseAllowlistEntry :: String -> Either String AllowlistEntry
parseAllowlistEntry raw =
  case map trim (splitOnDoubleColon (trim raw)) of
    [file]
      | not (null file) ->
          Right
            AllowlistEntry
              { allowlistEntryFile = file,
                allowlistEntryClassName = Nothing,
                allowlistEntryMethodName = Nothing
              }
    [file, className]
      | not (null file),
        not (null className) ->
          Right
            AllowlistEntry
              { allowlistEntryFile = file,
                allowlistEntryClassName = Just className,
                allowlistEntryMethodName = Nothing
              }
    [file, className, methodName]
      | not (null file),
        not (null className),
        not (null methodName) ->
          Right
            AllowlistEntry
              { allowlistEntryFile = file,
                allowlistEntryClassName = Just className,
                allowlistEntryMethodName = Just methodName
              }
    _ -> Left ("Invalid allowlist entry: " ++ raw)
  where
    trim = dropWhile isSpace . dropWhileEnd isSpace

    splitOnDoubleColon text =
      case breakOnDoubleColon text of
        (headPart, "") -> [headPart]
        (headPart, rest) -> headPart : splitOnDoubleColon (drop 2 rest)

    breakOnDoubleColon text =
      case text of
        ':' : ':' : rest -> ("", ':' : ':' : rest)
        character : rest ->
          let (headPart, tailPart) = breakOnDoubleColon rest
           in (character : headPart, tailPart)
        [] -> ("", "")
