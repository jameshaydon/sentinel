<script lang="ts">
	import { sentinel } from '$lib/websocket.svelte';

	let debugContainer: HTMLDivElement;

	$effect(() => {
		// Track length to trigger scroll on new entries
		sentinel.debugEntries.length;
		if (debugContainer) {
			debugContainer.scrollTop = debugContainer.scrollHeight;
		}
	});

	function formatTime(d: Date): string {
		return d.toLocaleTimeString('en-GB', { hour12: false });
	}
</script>

<div class="flex h-full flex-col bg-gray-950">
	<div class="flex items-center justify-between border-b border-gray-800 px-4 py-2">
		<h2 class="text-sm font-semibold text-gray-400">Debug Log</h2>
		<span class="rounded-full bg-gray-800 px-2 py-0.5 text-xs text-gray-500">
			{sentinel.debugEntries.length}
		</span>
	</div>

	<div bind:this={debugContainer} class="flex-1 overflow-y-auto p-3 font-mono text-xs">
		{#if sentinel.debugEntries.length === 0}
			<p class="italic text-gray-600">Waiting for events...</p>
		{:else}
			{#each sentinel.debugEntries as entry (entry.id)}
				<div class="mb-1 border-b border-gray-900 pb-1">
					<span class="text-gray-600">{formatTime(entry.timestamp)}</span>
					<pre class="mt-0.5 whitespace-pre-wrap text-green-400">{entry.text}</pre>
				</div>
			{/each}
		{/if}
	</div>
</div>
