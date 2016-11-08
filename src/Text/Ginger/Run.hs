{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TupleSections #-}
{-#LANGUAGE TypeSynonymInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE ScopedTypeVariables #-}
-- | Execute Ginger templates in an arbitrary monad.
--
-- Usage example:
--
-- > render :: Template -> Text -> Text -> Text
-- > render template -> username imageURL = do
-- >    let contextLookup varName =
-- >            case varName of
-- >                "username" -> toGVal username
-- >                "imageURL" -> toGVal imageURL
-- >                _ -> def -- def for GVal is equivalent to a NULL value
-- >        context = makeContext contextLookup
-- >    in htmlSource $ runGinger context template
module Text.Ginger.Run
( runGingerT
, runGinger
, GingerContext
, makeContext
, makeContextM
, makeContext'
, makeContextM'
, makeContextHtml
, makeContextHtmlM
, makeContextText
, makeContextTextM
, Run, liftRun, liftRun2
, extractArgs, extractArgsT, extractArgsL, extractArgsDefL
)
where

import Prelude ( (.), ($), (==), (/=)
               , (>), (<), (>=), (<=)
               , (+), (-), (*), (/), div, (**), (^)
               , (||), (&&)
               , (++)
               , Show, show
               , undefined, otherwise
               , Maybe (..)
               , Bool (..)
               , Int, Integer, String
               , fromIntegral, floor, round
               , not
               , show
               , uncurry
               , seq
               , fst, snd
               , maybe
               , Either (..)
               , id
               )
import qualified Prelude
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.List as List
import Text.Ginger.AST
import Text.Ginger.Html
import Text.Ginger.GVal
import Text.Ginger.Run.Type
import Text.Ginger.Run.Builtins
import Text.Ginger.Run.FuncUtils
import Text.Ginger.Run.VM
import Text.Printf
import Text.PrintfA
import Text.Ginger.Parse (parseGinger, ParserError)
import Control.Monad.Except (runExceptT, throwError, catchError)

import Data.Text (Text)
import Data.String (fromString)
import qualified Data.Text as Text
import qualified Data.ByteString.UTF8 as UTF8
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Scientific (Scientific, formatScientific)
import qualified Data.Scientific as Scientific
import Data.Default (def)
import Safe (readMay, lastDef, headMay)
import Network.HTTP.Types (urlEncode)
import Debug.Trace (trace)
import Data.List (lookup, zipWith, unzip)

defaultScope :: forall m h. (Monoid h, Monad m, ToGVal (Run m h) h) => [(Text, GVal (Run m h))]
defaultScope =
    [ ("raw", fromFunction gfnRawHtml)
    , ("abs", fromFunction . unaryNumericFunc 0 $ Prelude.abs)
    , ("any", fromFunction gfnAny)
    , ("all", fromFunction gfnAll)
    -- TODO: batch
    , ("capitalize", fromFunction . variadicStringFunc $ mconcat . Prelude.map capitalize)
    , ("ceil", fromFunction . unaryNumericFunc 0 $ Prelude.fromIntegral . Prelude.ceiling)
    , ("center", fromFunction gfnCenter)
    , ("concat", fromFunction . variadicStringFunc $ mconcat)
    , ("contains", fromFunction gfnContains)
    , ("d", fromFunction gfnDefault)
    , ("date", fromFunction gfnDateFormat)
    , ("dateformat", fromFunction gfnDateFormat)
    , ("default", fromFunction gfnDefault)
    , ("dictsort", fromFunction gfnDictsort)
    , ("difference", fromFunction . variadicNumericFunc 0 $ difference)
    , ("e", fromFunction gfnEscape)
    , ("equals", fromFunction gfnEquals)
    , ("escape", fromFunction gfnEscape)
    , ("eval", fromFunction gfnEval)
    , ("filesizeformat", fromFunction gfnFileSizeFormat)
    , ("filter", fromFunction gfnFilter)
    , ("floor", fromFunction . unaryNumericFunc 0 $ Prelude.fromIntegral . Prelude.floor)
    , ("format", fromFunction gfnPrintf)
    , ("greater", fromFunction gfnGreater)
    , ("greaterEquals", fromFunction gfnGreaterEquals)
    , ("int", fromFunction . unaryFunc $ toGVal . fmap (Prelude.truncate :: Scientific -> Int) . asNumber)
    , ("int_ratio", fromFunction . variadicNumericFunc 1 $ fromIntegral . intRatio . Prelude.map Prelude.floor)
    , ("iterable", fromFunction . unaryFunc $ toGVal . (\x -> isList x || isDict x))
    , ("length", fromFunction . unaryFunc $ toGVal . length)
    , ("less", fromFunction gfnLess)
    , ("lessEquals", fromFunction gfnLessEquals)
    , ("modulo", fromFunction . variadicNumericFunc 1 $ fromIntegral . modulo . Prelude.map Prelude.floor)
    , ("nequals", fromFunction gfnNEquals)
    , ("num", fromFunction . unaryFunc $ toGVal . asNumber)
    , ("printf", fromFunction gfnPrintf)
    , ("product", fromFunction . variadicNumericFunc 1 $ Prelude.product)
    , ("ratio", fromFunction . variadicNumericFunc 1 $ Scientific.fromFloatDigits . ratio . Prelude.map Scientific.toRealFloat)
    , ("replace", fromFunction gfnReplace)
    , ("round", fromFunction . unaryNumericFunc 0 $ Prelude.fromIntegral . Prelude.round)
    , ("show", fromFunction . unaryFunc $ fromString . show)
    , ("slice", fromFunction gfnSlice)
    , ("sort", fromFunction gfnSort)
    , ("str", fromFunction . unaryFunc $ toGVal . asText)
    , ("sum", fromFunction . variadicNumericFunc 0 $ Prelude.sum)
    , ("truncate", fromFunction . unaryNumericFunc 0 $ Prelude.fromIntegral . Prelude.truncate)
    , ("urlencode", fromFunction gfnUrlEncode)
    ]

-- | Purely expand a Ginger template. The underlying carrier monad is 'Writer'
-- 'h', which is used to collect the output and render it into a 'h'
-- value.
runGinger :: (ToGVal (Run (Writer h) h) h, Monoid h) => GingerContext (Writer h) h -> Template -> h
runGinger context template = execWriter $ runGingerT context template

-- | Monadically run a Ginger template. The @m@ parameter is the carrier monad.
runGingerT :: (ToGVal (Run m h) h, Monoid h, Monad m, Functor m) => GingerContext m h -> Template -> m (Either RuntimeError ())
runGingerT context tpl =
    runReaderT (evalStateT (runExceptT (runTemplate tpl)) (defRunState tpl)) context

-- | Find the effective base template of an inheritance chain
baseTemplate :: Template -> Template
baseTemplate t =
    case templateParent t of
        Nothing -> t
        Just p -> baseTemplate p

-- | Run a template.
runTemplate :: (ToGVal (Run m h) h, Monoid h, Monad m, Functor m) => Template -> Run m h ()
runTemplate = fmap (Prelude.const ()) . runStatement . templateBody . baseTemplate

-- | Run an action within a different template context.
withTemplate :: (Monad m, Functor m) => Template -> Run m h a -> Run m h a
withTemplate tpl a = do
    oldTpl <- gets rsCurrentTemplate
    oldBlockName <- gets rsCurrentBlockName
    modify (\s -> s { rsCurrentTemplate = tpl, rsCurrentBlockName = Nothing })
    result <- a
    modify (\s -> s { rsCurrentTemplate = oldTpl, rsCurrentBlockName = oldBlockName })
    return result

-- | Run an action within a block context
withBlockName :: (Monad m, Functor m) => VarName -> Run m h a -> Run m h a
withBlockName blockName a = do
    oldBlockName <- gets rsCurrentBlockName
    modify (\s -> s { rsCurrentBlockName = Just blockName })
    result <- a
    modify (\s -> s { rsCurrentBlockName = oldBlockName })
    return result

lookupBlock :: (Monad m, Functor m) => VarName -> Run m h Block
lookupBlock blockName = do
    tpl <- gets rsCurrentTemplate
    let blockMay = resolveBlock blockName tpl
    case blockMay of
        Nothing -> throwError $ UndefinedBlockError blockName
        Just block -> return block
    where
        resolveBlock :: VarName -> Template -> Maybe Block
        resolveBlock name tpl =
            case HashMap.lookup name (templateBlocks tpl) of
                Just block ->
                    return block -- Found it!
                Nothing ->
                    templateParent tpl >>= resolveBlock name

-- | Run one statement.
runStatement :: forall m h. (ToGVal (Run m h) h, Monoid h, Monad m, Functor m) => Statement -> Run m h ()
runStatement NullS = return ()
runStatement (MultiS xs) = forM_ xs runStatement
runStatement (LiteralS html) = echo (toGVal html)
runStatement (InterpolationS expr) = runExpression expr >>= echo
runStatement (IfS condExpr true false) = do
    cond <- runExpression condExpr
    runStatement $ if toBoolean cond then true else false

runStatement (SetVarS name valExpr) = do
    val <- runExpression valExpr
    setVar name val

runStatement (DefMacroS name macro) = do
    let val = macroToGVal macro
    setVar name val

runStatement (BlockRefS blockName) = do
    block <- lookupBlock blockName
    withBlockName blockName $
        runStatement (blockBody block)

runStatement (ScopedS body) = withLocalScope runInner
    where
        runInner :: (Functor m, Monad m) => Run m h ()
        runInner = runStatement body

runStatement (ForS varNameIndex varNameValue itereeExpr body) = do
    let go :: Int -> GVal (Run m h) -> Run m h (GVal (Run m h))
        go recursionDepth iteree = do
            let iterPairs =
                    if isJust (asDictItems iteree)
                        then [ (toGVal k, v) | (k, v) <- fromMaybe [] (asDictItems iteree) ]
                        else Prelude.zip (Prelude.map toGVal ([0..] :: [Int])) (fromMaybe [] (asList iteree))
                numItems :: Int
                numItems = Prelude.length iterPairs
                cycle :: Int -> [(Maybe Text, GVal (Run m h))] -> Run m h (GVal (Run m h))
                cycle index args = return
                                 . fromMaybe def
                                 . headMay
                                 . Prelude.drop (index `Prelude.mod` Prelude.length args)
                                 . fmap snd
                                 $ args
                loop :: [(Maybe Text, GVal (Run m h))] -> Run m h (GVal (Run m h))
                loop [] = throwError $ ArgumentsError "loop" "at least one argument is required"
                loop ((_, loopee):_) = go (Prelude.succ recursionDepth) loopee
                iteration :: (Int, (GVal (Run m h), GVal (Run m h))) -> Run m h ()
                iteration (index, (key, value)) = do
                    setVar varNameValue value
                    setVar "loop" $
                        (dict [ "index" ~> Prelude.succ index
                             , "index0" ~> index
                             , "revindex" ~> (numItems - index)
                             , "revindex0" ~> (numItems - index - 1)
                             , "depth" ~> Prelude.succ recursionDepth
                             , "depth0" ~> recursionDepth
                             , "first" ~> (index == 0)
                             , "last" ~> (Prelude.succ index == numItems)
                             , "length" ~> numItems
                             , "cycle" ~> fromFunction (cycle index)
                             ])
                             { asFunction = Just loop }
                    case varNameIndex of
                        Nothing -> return ()
                        Just n -> setVar n key
                    runStatement body
            withLocalScope $ forM_ (Prelude.zip [0..] iterPairs) iteration
            return def
    runExpression itereeExpr >>= go 0 >> return ()

runStatement (PreprocessedIncludeS tpl) =
    withTemplate tpl $ runTemplate tpl

runStatement (TryCatchS tryS catchS finallyS) = do
    result <- (runStatement tryS) `catchError` handle
    runStatement finallyS
    return result
    where
        handle e = withLocalScope $ do
            setVar "exception" (toGVal e)
            runStatement catchS

-- | Deeply magical function that converts a 'Macro' into a Function.
macroToGVal :: forall m h. (ToGVal (Run m h) h, Monoid h, Functor m, Monad m) => Macro -> GVal (Run m h)
macroToGVal (Macro argNames body) =
    fromFunction f
    where
        f :: Function (Run m h)
        -- Establish a local state to not contaminate the parent scope
        -- with function arguments and local variables, and;
        -- Establish a local context, where we override the HTML writer,
        -- rewiring it to append any output to the state's capture.
        f args =
            withLocalState . local (\c -> c { contextWrite = appendCapture }) $ do
                clearCapture
                forM_ (HashMap.toList matchedArgs) (uncurry setVar)
                setVar "varargs" . toGVal $ positionalArgs
                setVar "kwargs" . toGVal $ namedArgs
                runStatement body
                -- At this point, we're still inside the local state, so the
                -- capture contains the macro's output; we now simply return
                -- the capture as the function's return value.
                toGVal <$> fetchCapture
                where
                    matchArgs' :: [(Maybe Text, GVal (Run m h))] -> (HashMap Text (GVal (Run m h)), [GVal (Run m h)], HashMap Text (GVal (Run m h)))
                    matchArgs' = matchFuncArgs argNames
                    (matchedArgs, positionalArgs, namedArgs) = matchArgs' args


-- | Run (evaluate) an expression and return its value into the Run monad
runExpression (StringLiteralE str) = return . toGVal $ str
runExpression (NumberLiteralE n) = return . toGVal $ n
runExpression (BoolLiteralE b) = return . toGVal $ b
runExpression NullLiteralE = return def
runExpression (VarE key) = getVar key
runExpression (ListE xs) = toGVal <$> forM xs runExpression
runExpression (ObjectE xs) = do
    items <- forM xs $ \(a, b) -> do
        l <- asText <$> runExpression a
        r <- runExpression b
        return (l, r)
    return . toGVal . HashMap.fromList $ items
runExpression (MemberLookupE baseExpr indexExpr) = do
    base <- runExpression baseExpr
    index <- runExpression indexExpr
    return . fromMaybe def . lookupLoose index $ base
runExpression (CallE funcE argsEs) = do
    args <- forM argsEs $
        \(argName, argE) -> (argName,) <$> runExpression argE
    func <- toFunction <$> runExpression funcE
    case func of
        Nothing -> return def
        Just f -> f args
runExpression (LambdaE argNames body) = do
    let fn args = withLocalScope $ do
            forM_ (Prelude.zip argNames (fmap snd args)) $ uncurry setVar
            runExpression body
    return $ fromFunction fn
runExpression (TernaryE condition yes no) = do
    condVal <- runExpression condition
    let expr = if asBoolean condVal then yes else no
    runExpression expr

-- | Helper function to output a HTML value using whatever print function the
-- context provides.
echo :: (Monad m, Functor m) => GVal (Run m h) -> Run m h ()
echo src = do
    e <- asks contextEncode
    p <- asks contextWrite
    p . e $ src

defRunState :: forall m h. (ToGVal (Run m h) h, Monoid h, Monad m)
            => Template
            -> RunState m h
defRunState tpl =
    RunState
        { rsScope = HashMap.fromList defaultScope
        , rsCapture = mempty
        , rsCurrentTemplate = tpl
        , rsCurrentBlockName = Nothing
        }

gfnEval :: (Monad m, Monoid h, ToGVal (Run m h) h) => Function (Run m h)
gfnEval args =
    let extracted =
            extractArgsDefL
                [ ("src", def)
                , ("context", def)
                ]
                args
    in case extracted of
        Left _ -> throwError $ ArgumentsError "eval" "expected: (src, context)"
        Right [gSrc, gContext] -> do
            result <- parseGinger
                (Prelude.const . return $ Nothing) -- include resolver
                Nothing -- source name
                (Text.unpack . asText $ gSrc) -- source code
            tpl <- case result of
                Left err -> throwError $ EvalParseError err
                Right t -> return t
            let localLookup varName = return $
                    lookupLooseDef def (toGVal varName) gContext
                localContext c = c
                    { contextWrite = appendCapture
                    , contextLookup = localLookup
                    }
            withLocalState $ do
                put $ defRunState tpl
                local localContext $ do
                    clearCapture
                    runStatement $ templateBody tpl
                    -- At this point, we're still inside the local state, so the
                    -- capture contains the macro's output; we now simply return
                    -- the capture as the function's return value.
                    toGVal <$> fetchCapture
