<template>
  <div 
    class="device" 
    data-device="iPhone7" 
    data-orientation="portrait" 
    data-color="black"
  >
    <div class="screen">
      <slot />
    </div>
  </div>
</template>

<script>
export default {
}
</script>

<style scoped>
.device .screen img {
  padding: 0;
}
</style>
