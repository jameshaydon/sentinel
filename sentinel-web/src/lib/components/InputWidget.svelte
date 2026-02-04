<script lang="ts">
	import { sentinel } from '$lib/websocket.svelte';

	let textInput = $state('');

	function submit(value: string) {
		sentinel.sendInputResponse(value);
		textInput = '';
	}

	function handleKeydown(e: KeyboardEvent) {
		if (e.key === 'Enter' && textInput.trim()) {
			submit(textInput.trim());
		}
	}
</script>

{#if sentinel.inputRequest}
	{@const req = sentinel.inputRequest}
	<div class="border-t border-amber-300 bg-amber-50 px-4 py-3">
		<p class="mb-2 text-sm font-medium text-amber-900">{req.question}</p>

		{#if req.inputKind === 'AskableInputKind'}
			<div class="flex gap-2">
				<button
					onclick={() => submit('yes')}
					class="rounded-md bg-green-600 px-4 py-1.5 text-sm font-medium text-white hover:bg-green-700"
				>
					Yes
				</button>
				<button
					onclick={() => submit('no')}
					class="rounded-md bg-red-600 px-4 py-1.5 text-sm font-medium text-white hover:bg-red-700"
				>
					No
				</button>
			</div>
		{:else if req.candidates.length > 0}
			<div class="flex flex-wrap gap-2">
				{#each req.candidates as candidate}
					<button
						onclick={() => submit(candidate)}
						class="rounded-md bg-blue-600 px-3 py-1.5 text-sm font-medium text-white hover:bg-blue-700"
					>
						{candidate}
					</button>
				{/each}
			</div>
		{:else}
			<div class="flex gap-2">
				<input
					type="text"
					bind:value={textInput}
					onkeydown={handleKeydown}
					placeholder="Type your answer..."
					class="flex-1 rounded-md border border-gray-300 px-3 py-1.5 text-sm focus:border-blue-500 focus:ring-1 focus:ring-blue-500 focus:outline-none"
				/>
				<button
					onclick={() => submit(textInput.trim())}
					disabled={!textInput.trim()}
					class="rounded-md bg-blue-600 px-4 py-1.5 text-sm font-medium text-white hover:bg-blue-700 disabled:opacity-50"
				>
					Send
				</button>
			</div>
		{/if}
	</div>
{/if}
