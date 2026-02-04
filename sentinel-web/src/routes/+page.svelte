<script lang="ts">
	import { onMount } from 'svelte';
	import { sentinel } from '$lib/websocket.svelte';
	import ChatPane from '$lib/components/ChatPane.svelte';
	import DebugPane from '$lib/components/DebugPane.svelte';

	const port = new URLSearchParams(window.location.search).get('port') ?? '9090';
	const WS_URL = `ws://localhost:${port}`;

	onMount(() => {
		sentinel.connect(WS_URL);
		return () => sentinel.disconnect();
	});

	const statusColor: Record<string, string> = {
		connecting: 'bg-yellow-400',
		connected: 'bg-green-500',
		disconnected: 'bg-gray-400',
		error: 'bg-red-500'
	};
</script>

<div class="flex h-screen flex-col">
	<!-- Header -->
	<header class="flex items-center justify-between border-b border-gray-200 bg-white px-4 py-2">
		<h1 class="text-lg font-bold text-gray-800">Sentinel</h1>
		<div class="flex items-center gap-2">
			<span class={`inline-block h-2.5 w-2.5 rounded-full ${statusColor[sentinel.connectionStatus]}`}></span>
			<span class="text-xs text-gray-500 capitalize">{sentinel.connectionStatus}</span>
		</div>
	</header>

	<!-- Main content -->
	<div class="flex min-h-0 flex-1">
		<!-- Debug pane (hidden on mobile) -->
		<div class="hidden w-1/2 border-r border-gray-200 md:block">
			<DebugPane />
		</div>

		<!-- Chat pane -->
		<div class="w-1/2 min-w-0 flex-1 md:flex-none">
			<ChatPane />
		</div>
	</div>
</div>
