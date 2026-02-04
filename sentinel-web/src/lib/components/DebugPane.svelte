<script lang="ts">
	import type { Scalar } from '$lib/types';
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

	function formatScalar(s: Scalar): string {
		switch (s.tag) {
			case 'ScBool':
				return s.contents ? 'yes' : 'no';
			case 'ScNum':
				return String(s.contents);
			case 'ScStr':
				return s.contents;
			case 'ScExpr': {
				const [name, args] = s.contents;
				if (args.length === 0) return name;
				return `${name}(${args.map(formatScalar).join(', ')})`;
			}
		}
	}

	function formatArgs(args: Scalar[]): string {
		return args.map(formatScalar).join(', ');
	}

	let factEntries = $derived(
		sentinel.factStore
			? Object.entries(sentinel.factStore.factsByPredicate).sort(([a], [b]) =>
					a.localeCompare(b)
				)
			: []
	);
</script>

<div class="flex h-full flex-col bg-gray-950">
	<!-- Fact Store Panel -->
	<div class="flex min-h-0 flex-col border-b border-gray-800">
		<div class="flex items-center justify-between px-4 py-2">
			<h2 class="text-sm font-semibold text-gray-400">Fact Store</h2>
			<span class="rounded-full bg-gray-800 px-2 py-0.5 text-xs text-gray-500">
				{factEntries.length}
			</span>
		</div>
		<div class="max-h-64 overflow-y-auto p-3 pt-0 font-mono text-xs">
			{#if factEntries.length === 0}
				<p class="italic text-gray-700">No facts established yet.</p>
			{:else}
				{#each factEntries as [predName, facts]}
					<div class="mb-1.5">
						<span class="text-cyan-400">{predName}</span>
						{#each facts as fact}
							<div class="ml-3 text-gray-400">
								({formatArgs(fact.arguments)})
							</div>
						{/each}
					</div>
				{/each}
			{/if}
		</div>
	</div>

	<!-- Debug Log Panel -->
	<div class="flex min-h-0 flex-1 flex-col">
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
					<details open class="mb-1 border-b border-gray-900 pb-1">
						<summary class="cursor-pointer select-none text-gray-600">
							{formatTime(entry.timestamp)}
							<span class="ml-1 text-gray-500"
								>{entry.text.split('\n')[0].slice(0, 80)}{entry.text.length > 80 ||
								entry.text.includes('\n')
									? '…'
									: ''}</span
							>
						</summary>
						<pre class="mt-0.5 whitespace-pre-wrap text-green-400">{entry.text}</pre>
					</details>
				{/each}
			{/if}
		</div>
	</div>
</div>
