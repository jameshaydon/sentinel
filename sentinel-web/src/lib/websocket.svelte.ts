import type {
	BaseFactStore,
	ChatMessage,
	ClientMessage,
	ConnectionStatus,
	DebugEntry,
	InputMeta,
	ServerMessage
} from './types';

let nextId = 0;
function uid(): string {
	return String(nextId++);
}

class SentinelConnection {
	chatMessages: ChatMessage[] = $state([]);
	debugEntries: DebugEntry[] = $state([]);
	connectionStatus: ConnectionStatus = $state('disconnected');
	inputRequest: InputMeta | null = $state(null);
	isWaitingForResponse: boolean = $state(false);
	factStore: BaseFactStore | null = $state(null);

	private ws: WebSocket | null = null;

	connect(url: string) {
		this.disconnect();
		this.connectionStatus = 'connecting';

		const ws = new WebSocket(url);
		this.ws = ws;

		ws.onopen = () => {
			this.connectionStatus = 'connected';
		};

		ws.onclose = () => {
			this.connectionStatus = 'disconnected';
			this.isWaitingForResponse = false;
			this.ws = null;
		};

		ws.onerror = () => {
			this.connectionStatus = 'error';
			this.isWaitingForResponse = false;
		};

		ws.onmessage = (event) => {
			const msg: ServerMessage = JSON.parse(event.data);
			this.handleMessage(msg);
		};
	}

	disconnect() {
		if (this.ws) {
			this.ws.close();
			this.ws = null;
		}
		this.connectionStatus = 'disconnected';
		this.isWaitingForResponse = false;
		this.inputRequest = null;
	}

	sendChat(text: string) {
		if (!this.ws || this.ws.readyState !== WebSocket.OPEN) return;

		const msg: ClientMessage = { tag: 'UserChat', contents: text };
		this.ws.send(JSON.stringify(msg));

		this.chatMessages.push({
			id: uid(),
			role: 'user',
			content: text,
			timestamp: new Date()
		});
		this.isWaitingForResponse = true;
	}

	sendInputResponse(text: string) {
		if (!this.ws || this.ws.readyState !== WebSocket.OPEN) return;

		const msg: ClientMessage = { tag: 'InputResponse', contents: text };
		this.ws.send(JSON.stringify(msg));
		this.inputRequest = null;
	}

	private handleMessage(msg: ServerMessage) {
		switch (msg.tag) {
			case 'DebugEvent':
				this.debugEntries.push({
					id: uid(),
					text: msg.contents,
					timestamp: new Date()
				});
				this.chatMessages.push({
					id: uid(),
					role: 'system',
					content: msg.contents,
					timestamp: new Date(),
					isDebug: true
				});
				break;

			case 'ChatResponse':
				this.chatMessages.push({
					id: uid(),
					role: 'assistant',
					content: msg.contents,
					timestamp: new Date()
				});
				this.isWaitingForResponse = false;
				break;

			case 'InputRequest':
				this.inputRequest = msg.contents;
				this.isWaitingForResponse = false;
				break;

			case 'Ready':
				this.chatMessages.push({
					id: uid(),
					role: 'system',
					content: msg.contents,
					timestamp: new Date()
				});
				break;

			case 'ServerError':
				this.chatMessages.push({
					id: uid(),
					role: 'system',
					content: msg.contents,
					timestamp: new Date(),
					isError: true
				});
				this.isWaitingForResponse = false;
				break;

			case 'FactStoreUpdate':
				this.factStore = msg.contents;
				break;
		}
	}
}

export const sentinel = new SentinelConnection();
