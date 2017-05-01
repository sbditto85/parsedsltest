module Generators where


import qualified Data.List                   as L
import           Lib
import           Test.QuickCheck


replaceSlash :: [Char] -> [Char]
replaceSlash ('\\':'\"':xs) = '\"' : replaceSlash xs
replaceSlash ('\\':'\\':xs) = '\\' : replaceSlash xs
replaceSlash ('\\':'n':xs) = '\n' : replaceSlash xs
replaceSlash (x:xs) = x : replaceSlash xs
replaceSlash [] = []

trimQuotes :: [Char] -> [Char]
trimQuotes = reverse . (L.drop 1) . reverse . (L.drop 1)

escapedCharToChar :: [Char] -> Char
escapedCharToChar "\\\"" = '\"'
escapedCharToChar "\\\\" = '\\'
escapedCharToChar "\\n" = '\n'
escapedCharToChar _ = '\0'

genLetter :: Gen Char
genLetter = elements (['A'..'Z'] ++ ['a'..'z'])

genDigit :: Gen Char
genDigit = elements ['0'..'9']

genDigits :: Gen Word
genDigits = arbitrary

genIdentSymbol :: Gen Char
genIdentSymbol = elements ['-', '_']


genMixed :: Gen [Char]
genMixed = listOf1 $ oneof [ genDigit, genIdentSymbol, genLetter ]

genIdent :: Gen [Char]
genIdent = do
  f <- genLetter
  rest <- genMixed
  pure $ f : rest

genBadIdent :: Gen [Char]
genBadIdent = do
  f <- oneof [ genDigit, genIdentSymbol ]
  rest <- genMixed
  pure $ f : rest


-- | gen assignement stuff
genAssignment :: Gen Assignment
genAssignment =
  oneof [ Assignment <$> (Ident <$> genIdent)
        , pure NoAssignment
        ]

assignmentToString :: Assignment -> String
assignmentToString (Assignment (Ident str)) = str ++ " <- "
assignmentToString NoAssignment = ""

genStringSymbol :: Gen Char
genStringSymbol = elements [' ', '!', '#', '$', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', '/', ':', ';', '<', '=', '>', '?', '@', '[', ']', '^', '_', '`', '{', '|', '}', '~']

genEscaped :: Gen [Char]
genEscaped = oneof [ pure "\\\\"
                   , pure "\\\""
                   , pure "\\n"
                   ]

genEscapedString :: Gen [Char]
genEscapedString =
  fmap mconcat $ listOf1 $ oneof [ listOf $ oneof [ genDigit, genLetter, genStringSymbol ]
                                 , genEscaped
                                 ]

genString :: Gen [Char]
genString =
  let
    begQuoteAndStr = (++) <$> (pure "\"") <*> genEscapedString
    lastChar = (:[]) <$> oneof [ genDigit, genLetter ]
    withLastChar = (++) <$> begQuoteAndStr <*> lastChar
  in
    (++) <$> withLastChar <*> (pure "\"")


-- | String Concat generator stuff
genStringConcat :: Gen StringConcat
genStringConcat =
  oneof [ genStrIdentConcat
        , genStrConcat
        , genStrIdent
        , genStrString
        ]
  where
    genStrIdentConcat = 
      let
        ident' = Ident <$> genIdent
        strConcat = genStringConcat
      in
        StrIdentConcat <$> ident' <*> strConcat

    genStrConcat =
      let
        str = genString
        strConcat = genStringConcat
      in
        StrConcat <$> str <*> strConcat

    genStrIdent = StrIdent <$> (Ident <$> genIdent)
    genStrString = StrString <$> genString

stringConcatToString :: StringConcat -> String
stringConcatToString (StrIdentConcat (Ident ident') rest) = ident' ++ " ++ " ++ (stringConcatToString rest)
stringConcatToString (StrConcat str rest) = str ++ " ++ " ++ (stringConcatToString rest)
stringConcatToString (StrIdent (Ident ident')) = ident'
stringConcatToString (StrString str) = str

genStrConcatToParsed :: StringConcat -> StringConcat
genStrConcatToParsed (StrIdentConcat ident' rest) = StrIdentConcat ident' (genStrConcatToParsed rest)
genStrConcatToParsed (StrConcat str rest) = StrConcat (trimQuotes $ replaceSlash str) (genStrConcatToParsed rest)
genStrConcatToParsed strIdent@(StrIdent _) = strIdent
genStrConcatToParsed (StrString str) = StrString (trimQuotes $ replaceSlash str)


-- | declaration generator stuff
genDeclaration :: Gen Declaration
genDeclaration = do
  Declaration <$> (Ident <$> genIdent) <*> genStringConcat

declarationToString :: Declaration -> String
declarationToString (Declaration (Ident i) strConcat) = i ++ " = " ++ (stringConcatToString strConcat) ++ ";"

genDeclarationToParsed :: Declaration -> Declaration
genDeclarationToParsed (Declaration i strConcat) = Declaration i $ genStrConcatToParsed strConcat 


-- | httpGet generator stuff
genHttpGet :: Gen HttpGet
genHttpGet = HttpGet <$> genStringConcat

httpGetToString :: HttpGet -> String
httpGetToString (HttpGet strConcat) = "httpGet " ++ stringConcatToString strConcat

genHttpGetToParsed :: HttpGet -> HttpGet
genHttpGetToParsed (HttpGet strConcat) = HttpGet $ genStrConcatToParsed strConcat

-- | method generator stuff
genMethod :: Gen Method
genMethod =
  oneof [ genMethodHttpGet
        ]
  where
    genMethodHttpGet = MethodHttpGet <$> genHttpGet

methodToString :: Method -> String
methodToString (MethodHttpGet httpGet') = httpGetToString httpGet'

genMethodToParsed :: Method -> Method
genMethodToParsed (MethodHttpGet httpGet') = MethodHttpGet $ genHttpGetToParsed httpGet'


-- | errorHandler generator stuff
genErrorHandler :: Gen ErrorHandler
genErrorHandler =
  oneof [ genErrorHandlerLog
        , genErrorHandlerEmail
        ]
  where
    genErrorHandlerLog = ErrorHandlerLog <$> genStringConcat

    genErrorHandlerEmail = ErrorHandlerEmail <$> genStringConcat

errorHandlerToString :: ErrorHandler -> String
errorHandlerToString (ErrorHandlerLog strConcat) = "log " ++ stringConcatToString strConcat
errorHandlerToString (ErrorHandlerEmail strConcat) = "email " ++ stringConcatToString strConcat

genErrorHandlerToParsed :: ErrorHandler -> ErrorHandler
genErrorHandlerToParsed (ErrorHandlerLog strConcat) = ErrorHandlerLog $ genStrConcatToParsed strConcat
genErrorHandlerToParsed (ErrorHandlerEmail strConcat) = ErrorHandlerEmail $ genStrConcatToParsed strConcat


-- | handleError generator stuff
genHandleError :: Gen HandleError
genHandleError = HandleError <$> genDigits <*> genErrorHandler

genHandleErrors :: Gen [ HandleError ]
genHandleErrors = vectorOf 2 $ genHandleError
-- genHandleErrors = listOf1 $ genHandleError -- Takes a few seconds but works

handleErrorToString :: HandleError -> String
handleErrorToString (HandleError status errorHandler') = ": handleError " ++ show status ++ " " ++ errorHandlerToString errorHandler'

genHandleErrorToParsed :: HandleError -> HandleError
genHandleErrorToParsed (HandleError status errorHandler') = HandleError status $ genErrorHandlerToParsed errorHandler'

-- | action generator stuff
genAction :: Gen Action
genAction = Action <$> genAssignment <*> genMethod <*> genPossibleHandleErrors
  where
    genPossibleHandleErrors = oneof [ genHandleErrors
                                    , (:[]) <$> genHandleError
                                    , pure []
                                    ]

actionToString :: Action -> String
actionToString (Action{..}) = assignmentToString actionAssignment ++ methodToString actionMethod ++ (mconcat $ L.intersperse " " $ handleErrorToString <$> actionHandleError) ++ ";"

genActionToParsed :: Action -> Action
genActionToParsed (Action{..}) = Action actionAssignment (genMethodToParsed actionMethod) (genHandleErrorToParsed <$> actionHandleError)

-- | actions generator stuff
genActions :: Gen Actions
-- genActions = Actions <$> (listOf $ oneof [ IsAction <$> genAction  -- Takes a few seconds but works
genActions = Actions <$> (vectorOf 2 $ oneof [ IsAction <$> genAction
                                             , IsDeclaration <$> genDeclaration
                                             ]
                         )

actionsToString :: Actions -> String
actionsToString (Actions actionOrDeclarations) = mconcat $ L.intersperse "\n" $ actOrDecToString <$> actionOrDeclarations
  where
    actOrDecToString (IsAction action') = actionToString action'
    actOrDecToString (IsDeclaration declaration') = declarationToString declaration'

genActionsToParsed :: Actions -> Actions
genActionsToParsed (Actions actionOrDeclarations) = Actions $ genActOrGenDecToParsed <$> actionOrDeclarations
      where
        genActOrGenDecToParsed (IsAction action') = IsAction $ genActionToParsed action'
        genActOrGenDecToParsed (IsDeclaration declaration') = IsDeclaration $ genDeclarationToParsed declaration'

-- | WithCameraId generator stuff
genWithCameraId :: Gen WithCameraId
genWithCameraId = WithCameraId <$> (Ident <$> genIdent)

withCameraIdToString :: WithCameraId -> String
withCameraIdToString (WithCameraId (Ident i)) = "withCameraId " ++ i

-- | WithLiveUnitId generator stuff
genWithLiveUnitId :: Gen WithLiveUnitId
genWithLiveUnitId = WithLiveUnitId <$> (Ident <$> genIdent)

withLiveUnitIdToString :: WithLiveUnitId -> String
withLiveUnitIdToString (WithLiveUnitId (Ident i)) = "withLiveUnitId " ++ i

-- | InitFunc generator stuff
genInitFunc :: Gen InitFunc
genInitFunc = oneof [ InitWithCameraId <$> genWithCameraId
                    , InitWithLiveUnitId <$> genWithLiveUnitId
                    ]

initFuncToString :: InitFunc -> String
initFuncToString (InitWithCameraId withCameraId') = withCameraIdToString withCameraId'
initFuncToString (InitWithLiveUnitId withLiveUnitId') = withLiveUnitIdToString withLiveUnitId'

-- | JsonValue generator stuff
genJsonValue :: Gen JsonValue
genJsonValue = JsonValue <$> genStringConcat

jsonValueToString :: JsonValue -> String
jsonValueToString (JsonValue stringConcat') = stringConcatToString stringConcat'

genJsonValueToParsed :: JsonValue -> JsonValue
genJsonValueToParsed (JsonValue strConcat) = JsonValue $ genStrConcatToParsed strConcat


-- | JsonParam generator stuff
genJsonParam :: Gen JsonParam
genJsonParam = JsonParam <$> genMixed <*> genJsonValue

jsonParamToString :: JsonParam -> String
jsonParamToString (JsonParam key value) = "\"" ++ key ++ "\" : " ++ (jsonValueToString value)

genJsonParamToParsed :: JsonParam -> JsonParam
genJsonParamToParsed (JsonParam key value) = (JsonParam key (genJsonValueToParsed value))


-- | JsonParamList generator stuff
genJsonParamList :: Gen JsonParamList
genJsonParamList = JsonParamList <$> vectorOf 2 genJsonParam
-- genJsonParamList = JsonParamList <$> listOf1 genJsonParam -- works but takes longer

jsonParamListToString :: JsonParamList -> String
jsonParamListToString (JsonParamList jPList) = mconcat $ L.intersperse ", " $ jsonParamToString <$> jPList

genJsonParamListToParsed :: JsonParamList -> JsonParamList
genJsonParamListToParsed (JsonParamList jPList) = JsonParamList $ genJsonParamToParsed <$> jPList


-- | JsonObject generator stuff
genJsonObject :: Gen JsonObject
genJsonObject = JsonObject <$> genJsonParamList

jsonObjectToString :: JsonObject -> String
jsonObjectToString (JsonObject jsonParamList') = "{ " ++ (jsonParamListToString jsonParamList') ++ " }"

genJsonObjectToParsed :: JsonObject -> JsonObject
genJsonObjectToParsed (JsonObject jsonParamList') = JsonObject $ genJsonParamListToParsed jsonParamList'


-- | response generator stuffs
genResponse :: Gen Response
genResponse = Response <$> genDigits <*> genJsonObject

responseToString :: Response -> String
responseToString (Response status jsonObject') = "responseJson " ++ (show status) ++ (jsonObjectToString jsonObject')

genResponseToParsed :: Response -> Response
genResponseToParsed (Response status jsonObject') = Response status (genJsonObjectToParsed jsonObject')


-- | system call generator stuffs
genSystemCall :: Gen SystemCall
genSystemCall = SystemCall <$> genInitFunc <*> genActions <*> genResponse <*> genHandleErrors

systemCallToString :: SystemCall -> String
systemCallToString (SystemCall initFunc' actions' response' handleErrors')
  = (initFuncToString initFunc')
  ++ " { "
  ++ (actionsToString actions')
  ++ " } "
  ++ (responseToString response')
  ++ (mconcat $ L.intersperse " " $ handleErrorToString <$> handleErrors')

genSystemCallToParsed :: SystemCall -> SystemCall
genSystemCallToParsed (SystemCall initFunc' actions' response' handleErrors')
  = SystemCall
    initFunc'
    (genActionsToParsed actions')
    (genResponseToParsed response')
    (genHandleErrorToParsed <$> handleErrors')
