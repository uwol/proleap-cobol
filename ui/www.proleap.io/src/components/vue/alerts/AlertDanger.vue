<template>
  <div 
    class="alert alert-danger mb-4" 
    role="alert"
  >
    {{ text }}
  </div>
</template>

<script>
export default {
  props: ['text']
}
</script>

<style scoped>
.alert {
  border-radius: 0;
  text-align: left;
}
</style>
