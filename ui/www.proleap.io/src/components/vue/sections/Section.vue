<template>
  <section>
    <div 
      v-if="spacer" 
      class="spacer-top"
    />
    <div :class="{section: true, grey: spacer}">
      <slot />
    </div>
    <div 
      v-if="spacer" 
      class="spacer-bottom"
    />
  </section>
</template>

<script>
export default {
  props: ['spacer']
}
</script>

<style scoped>
.grey {
  background-color: #f5f8fa;
}

.section {
  padding: 5em 0;
}

.section .section--border-top {
  border-top: 1px solid #eee;
}

.spacer-top {
  background: linear-gradient(to top left, #f5f8fa 50%, #fff 50%);
  min-height: 3rem;
}

.spacer-bottom {
  background: linear-gradient(to top left, #fff 50%, #f5f8fa 50%);
  min-height: 3rem;
}
</style>
