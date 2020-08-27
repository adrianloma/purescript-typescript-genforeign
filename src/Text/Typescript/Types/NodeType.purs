module Text.Typesript.Types.NodeType where

import Data.Maybe
import Text.Typesript.Types.SyntaxKind
type ReadonlyTextRangeRep r =
  ( pos :: Int
  , end :: Int
  | r
  )

type TextRange r = ReadonlyTextRangeRep r

type NodeRep r =
  ( kind :: SyntaxKind
  , flags :: Int -- NodeFlags
  , id :: Maybe Int -- |  Unique id (used to look up NodeLinks)
  , parent :: String -- |  Parent node (initialized by binding)
  | ReadonlyTextRangeRep r
  )

type JSDocRep r =
  ( -- kind = SyntaxKind.JSDocComment <- must be this type
    parent :: String -- HasJSDoc;
  , tags :: Maybe Int -- NodeArray<JSDocTag>;
  , comment :: Maybe String
  | r
  )


{-
Goal #1:
Note: For some absurd reason, the enums do not correspond to their expected value.
For this reason, the 
"kind": 001 EndOfFileToken
"kind": 010 StringLiteral
"kind": 057 QuestionToken
"kind": 075 AmpersandAmpersandEqualsToken
"kind": 089
"kind": 100
"kind": 106
"kind": 110
"kind": 125
"kind": 128
"kind": 138
"kind": 140
"kind": 156
"kind": 158
"kind": 159
"kind": 161
"kind": 162
"kind": 173
"kind": 178
"kind": 187
"kind": 216
"kind": 245
"kind": 254
"kind": 255
"kind": 257
"kind": 258
"kind": 279
"kind": 290
"kind": 303
"kind": 306
"kind": 316

Goal #2:
"kind": 001
"kind": 008
"kind": 010
"kind": 013
"kind": 015
"kind": 016
"kind": 017
"kind": 027
"kind": 029
"kind": 031
"kind": 032
"kind": 033
"kind": 036
"kind": 037
"kind": 038
"kind": 039
"kind": 040
"kind": 041
"kind": 043
"kind": 044
"kind": 050
"kind": 053
"kind": 055
"kind": 056
"kind": 057
"kind": 058
"kind": 062
"kind": 063
"kind": 064
"kind": 073
"kind": 075
"kind": 089
"kind": 091
"kind": 100
"kind": 106
"kind": 110
"kind": 125
"kind": 128
"kind": 138
"kind": 140
"kind": 143
"kind": 146
"kind": 153
"kind": 154
"kind": 155
"kind": 156
"kind": 158
"kind": 159
"kind": 160
"kind": 161
"kind": 162
"kind": 169
"kind": 170
"kind": 173
"kind": 174
"kind": 178
"kind": 184
"kind": 187
"kind": 189
"kind": 191
"kind": 192
"kind": 193
"kind": 194
"kind": 195
"kind": 196
"kind": 197
"kind": 200
"kind": 201
"kind": 202
"kind": 204
"kind": 207
"kind": 208
"kind": 209
"kind": 210
"kind": 211
"kind": 213
"kind": 216
"kind": 217
"kind": 218
"kind": 221
"kind": 223
"kind": 225
"kind": 226
"kind": 227
"kind": 229
"kind": 230
"kind": 233
"kind": 234
"kind": 235
"kind": 237
"kind": 238
"kind": 242
"kind": 243
"kind": 244
"kind": 245
"kind": 246
"kind": 247
"kind": 249
"kind": 250
"kind": 251
"kind": 254
"kind": 255
"kind": 257
"kind": 258
"kind": 277
"kind": 278
"kind": 279
"kind": 281
"kind": 282
"kind": 283
"kind": 290
"kind": 303
"kind": 306
"kind": 316
"kind": 317
-}
