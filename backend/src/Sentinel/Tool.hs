-- | Representation of tools. Tools are monadic actions that can have effects.
module Sentinel.Tool
  ( Tool (..),
    Toolkit (..),
    ToolKind (..),
    toOpenAITool,
    queryTool,
    actionTool,
    trivialGuard,
  )
where

import Data.Aeson (Value)
import OpenAI.V1.Tool qualified as OpenAI
import Pre
import Sentinel.Guard (GuardM)

-- | Distinguishes query tools (read-only, produce facts) from action tools (mutating).
data ToolKind
  = -- | Query tools gather information and produce facts. They should be idempotent.
    QueryTool
  | -- | Action tools perform mutations. They may have guards that require facts.
    ActionTool
  deriving stock (Eq, Show)

-- | A tool bundles its schema with its execution logic.
-- 'm' is the monadic context (e.g., AgentM db).
-- 'fact' is the type of facts this tool can produce.
data Tool m fact = Tool
  { toolName :: Text,
    toolDescription :: Text,
    -- | Full JSON Schema for arguments
    toolParams :: Value,
    -- | Whether this is a query or action tool
    toolKind :: ToolKind,
    -- | Guard: LogicT-based guard that checks if tool invocation is allowed.
    -- The guard succeeds if at least one solution path completes.
    -- If all paths fail, the guard can report pending queries or questions
    -- that could unblock it.
    toolGuard :: Value -> GuardM fact (),
    -- | Logic: takes JSON args, performs effects. Throws on error.
    toolAction :: Value -> ExceptT Text m Text,
    -- | Facts produced after successful execution.
    -- For query tools, this extracts facts from the query result.
    -- For action tools, this is typically empty.
    toolFactsProduced :: Value -> m [fact]
  }

data Toolkit m fact = Toolkit
  { tools :: [Tool m fact],
    systemPrompt :: Text
  }

-- | Convert to OpenAI format using the tool's custom parameters
toOpenAITool :: Tool m fact -> OpenAI.Tool
toOpenAITool tool =
  OpenAI.Tool_Function
    { function =
        OpenAI.Function
          { name = tool.toolName,
            description = Just tool.toolDescription,
            parameters = Just tool.toolParams,
            strict = Nothing
          }
    }

-- | Create a query tool (trivial guard that always succeeds, produces facts).
queryTool ::
  Text ->
  Text ->
  Value ->
  (Value -> ExceptT Text m Text) ->
  (Value -> m [fact]) ->
  Tool m fact
queryTool name description params action factsProduced =
  Tool
    { toolName = name,
      toolDescription = description,
      toolParams = params,
      toolKind = QueryTool,
      toolGuard = \_ -> pure (), -- Trivial guard: always succeeds
      toolAction = action,
      toolFactsProduced = factsProduced
    }

-- | Create an action tool (has guard, produces no facts).
actionTool ::
  (Applicative m) =>
  Text ->
  Text ->
  Value ->
  (Value -> GuardM fact ()) ->
  (Value -> ExceptT Text m Text) ->
  Tool m fact
-- Note: Applicative constraint is for toolFactsProduced returning pure []
actionTool name description params guardFn action =
  Tool
    { toolName = name,
      toolDescription = description,
      toolParams = params,
      toolKind = ActionTool,
      toolGuard = guardFn,
      toolAction = action,
      toolFactsProduced = \_ -> pure []
    }

-- | A trivial guard that always succeeds.
trivialGuard :: Value -> GuardM fact ()
trivialGuard _ = pure ()
