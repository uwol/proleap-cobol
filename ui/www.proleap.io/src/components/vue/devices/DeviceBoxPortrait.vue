<template>
  <div class="device-container">
    <slot />
  </div>
</template>

<script>
export default {
}
</script>

<style scoped>
.device-container {
  margin-bottom: 2rem;
  margin-left: 3rem;
  margin-right: 3rem;
}

@media (max-width: 575px) {
  .device-container {
    margin-left: 8rem;
    margin-right: 8rem;
  }
}
</style>
