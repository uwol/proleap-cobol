<template>
  <pre class="code border rounded p-2 bg-white">{{ code }}</pre>
</template>

<script>
export default {
  props: ['code']
}
</script>

<style scoped>
pre.code {
  font-family: courier, "courier new", monospace;
  font-size: 0.8rem;
  font-weight: 500;
}
</style>
