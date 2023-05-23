<template>
  <div class="text-center">
    <img 
      :src="src" 
      :alt="alt" 
      class="img-fluid mb-2"
    >
    <p v-if="title">
      {{ title }}
    </p>
  </div>
</template>

<script>
export default {
  props: ['alt', 'src', 'title']
}
</script>

<style scoped>
img {
  max-width: 120px;
  max-height: 120px;
}
</style>
