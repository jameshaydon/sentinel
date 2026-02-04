# Sentinel Web Frontend

SvelteKit SPA for the Sentinel governance middleware. Connects to the Haskell backend via WebSocket.

You are able to use the Svelte MCP server, where you have access to comprehensive Svelte 5 and SvelteKit documentation. Here's how to use the available tools effectively:

## Available MCP Tools:

### 1. list-sections

Use this FIRST to discover all available documentation sections. Returns a structured list with titles, use_cases, and paths.
When asked about Svelte or SvelteKit topics, ALWAYS use this tool at the start of the chat to find relevant sections.

### 2. get-documentation

Retrieves full documentation content for specific sections. Accepts single or multiple sections.
After calling the list-sections tool, you MUST analyze the returned documentation sections (especially the use_cases field) and then use the get-documentation tool to fetch ALL documentation sections that are relevant for the user's task.

### 3. svelte-autofixer

Analyzes Svelte code and returns issues and suggestions.
You MUST use this tool whenever writing Svelte code before sending it to the user. Keep calling it until no issues or suggestions are returned.

### 4. playground-link

Generates a Svelte Playground link with the provided code.
After completing the code, ask the user if they want a playground link. Only call this tool after user confirmation and NEVER if code was written to files in their project.

## Commands

```bash
just frontend-dev      # Vite dev server at http://localhost:5173
just frontend-build    # Production build to build/
just frontend-check    # Type-check (svelte-check)
just frontend-install  # npm install
```

## Stack

- SvelteKit + TypeScript + Tailwind CSS v4 + adapter-static (SPA mode, `ssr = false`)
- Svelte 5 runes (`$state`, `$effect`, `$props`) — no legacy stores

## Architecture

- **WebSocket connection**: `ws://localhost:9090` by default, configurable via `?port=` query param
- **Singleton state**: `sentinel` export from `websocket.svelte.ts` — Svelte 5 runes class with reactive `chatMessages`, `debugEntries`, `connectionStatus`, `inputRequest`, `isWaitingForResponse`
- **Layout**: Two-pane on desktop (40% debug / 60% chat), chat-only on mobile with debug events inline as collapsible `<details>`

## WebSocket Protocol

Types in `src/lib/types.ts` must stay in sync with `backend/src/Sentinel/WebSocket.hs`.

Aeson Generic encoding: nullary sum constructors are plain strings, constructors with one field use `{ tag, contents }`.

- **ServerMessage**: `DebugEvent`, `ChatResponse`, `InputRequest`, `Ready`, `ServerError`
- **ClientMessage**: `UserChat`, `InputResponse`
- **InputMeta**: `{ question, inputKind, inputName, candidates }` — drives the InputWidget

## Components

- `ChatPane.svelte` — Scrolling message list, auto-scroll, user/assistant/system/debug/error message styling, "Thinking..." indicator
- `DebugPane.svelte` — Dark terminal aesthetic, monospace green text, timestamps, entry count
- `InputWidget.svelte` — Inline widgets based on `InputMeta.inputKind`:
  - `AskableInputKind` → Yes/No buttons
  - `ContextInputKind` with candidates → candidate value buttons
  - `ContextInputKind` without candidates → text input

## File Map

| File | Purpose |
|------|---------|
| `src/lib/types.ts` | TypeScript protocol types (mirrors `WebSocket.hs`) |
| `src/lib/websocket.svelte.ts` | WebSocket client + reactive state singleton |
| `src/lib/components/ChatPane.svelte` | Chat UI |
| `src/lib/components/DebugPane.svelte` | Debug log viewer |
| `src/lib/components/InputWidget.svelte` | Inline yes/no + candidate widgets |
| `src/routes/+page.svelte` | Main page + two-pane layout |
| `src/routes/+layout.ts` | SPA mode (`ssr = false`) |
| `svelte.config.js` | adapter-static with SPA fallback |
