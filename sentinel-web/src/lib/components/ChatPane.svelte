<script lang="ts">
	import { sentinel } from '$lib/websocket.svelte';
	import InputWidget from './InputWidget.svelte';

	let chatContainer: HTMLDivElement;
	let userInput = $state('');

	$effect(() => {
		sentinel.chatMessages.length;
		if (chatContainer) {
			chatContainer.scrollTop = chatContainer.scrollHeight;
		}
	});

	function sendMessage() {
		const text = userInput.trim();
		if (!text) return;
		sentinel.sendChat(text);
		userInput = '';
	}

	function handleKeydown(e: KeyboardEvent) {
		if (e.key === 'Enter' && !e.shiftKey) {
			e.preventDefault();
			sendMessage();
		}
	}

</script>

<div class="flex h-full flex-col bg-white">
	<!-- Messages -->
	<div bind:this={chatContainer} class="flex-1 overflow-y-auto p-4">
		{#each sentinel.chatMessages as msg (msg.id)}
			{#if msg.isDebug}
				<!-- Debug events only shown in the DebugPane -->
			{:else if msg.isError}
				<div class="mb-3 flex justify-start">
					<div class="max-w-[80%] rounded-lg bg-red-100 px-4 py-2 text-sm text-red-800">
						{msg.content}
					</div>
				</div>
			{:else if msg.role === 'user'}
				<div class="mb-3 flex justify-end">
					<div class="max-w-[80%] rounded-lg bg-blue-600 px-4 py-2 text-sm text-white">
						{msg.content}
					</div>
				</div>
			{:else if msg.role === 'assistant'}
				<div class="mb-3 flex justify-start">
					<div class="prose prose-sm max-w-[80%] rounded-lg bg-gray-100 px-4 py-2 text-sm">
						{msg.content}
					</div>
				</div>
			{:else}
				<div class="mb-3 flex justify-center">
					<div class="rounded-full bg-gray-200 px-3 py-1 text-xs text-gray-600">
						{msg.content}
					</div>
				</div>
			{/if}
		{/each}

		{#if sentinel.isWaitingForResponse}
			<div class="mb-3 flex justify-start">
				<div class="rounded-lg bg-gray-100 px-4 py-2 text-sm text-gray-400">
					<span class="inline-block animate-pulse">Thinking...</span>
				</div>
			</div>
		{/if}
	</div>

	<!-- Input widget (for side-session questions) -->
	<InputWidget />

	<!-- Chat input -->
	<div class="border-t border-gray-200 p-3">
		<div class="flex gap-2">
			<input
				type="text"
				bind:value={userInput}
				onkeydown={handleKeydown}
				placeholder={sentinel.inputRequest ? 'Answer the question above first...' : 'Type a message...'}
				disabled={!!sentinel.inputRequest || sentinel.connectionStatus !== 'connected'}
				class="flex-1 rounded-lg border border-gray-300 px-4 py-2 text-sm focus:border-blue-500 focus:ring-1 focus:ring-blue-500 focus:outline-none disabled:bg-gray-100 disabled:text-gray-400"
			/>
			<button
				onclick={sendMessage}
				disabled={!userInput.trim() || !!sentinel.inputRequest || sentinel.connectionStatus !== 'connected'}
				class="rounded-lg bg-blue-600 px-5 py-2 text-sm font-medium text-white hover:bg-blue-700 disabled:opacity-50"
			>
				Send
			</button>
		</div>
	</div>
</div>
