// Mirror of the WebSocket protocol types from the Haskell backend.
// Aeson Generic encoding: nullary constructors become plain strings,
// constructors with one field use { tag, contents }.

export type InputKind = 'ContextInputKind' | 'AskableInputKind';

export interface InputMeta {
	question: string;
	inputKind: InputKind;
	inputName: string;
	candidates: string[];
}

export type ServerMessage =
	| { tag: 'DebugEvent'; contents: string }
	| { tag: 'ChatResponse'; contents: string }
	| { tag: 'InputRequest'; contents: InputMeta }
	| { tag: 'Ready'; contents: string }
	| { tag: 'ServerError'; contents: string };

export type ClientMessage =
	| { tag: 'UserChat'; contents: string }
	| { tag: 'InputResponse'; contents: string };

export interface ChatMessage {
	id: string;
	role: 'user' | 'assistant' | 'system';
	content: string;
	timestamp: Date;
	isDebug?: boolean;
	isError?: boolean;
}

export interface DebugEntry {
	id: string;
	text: string;
	timestamp: Date;
}

export type ConnectionStatus = 'connecting' | 'connected' | 'disconnected' | 'error';
