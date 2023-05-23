<template>
  <div class="animation-hover">
    <slot />
  </div>
</template>

<script>
</script>

<style scoped>
.animation-hover {
  transition: 0.3s ease-in-out;
  box-shadow: 0 0 11px #eaf0f6;
}

.animation-hover:hover {
  box-shadow: 0 10px 50px -5px rgba(51,71,91,0.12);
  transform: scale(1.014);
}
</style>
