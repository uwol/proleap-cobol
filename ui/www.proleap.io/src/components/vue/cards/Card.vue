<template>
  <div class="card shadow-sm mb-3 p-2">
    <div class="card-body">
      <h1
        v-if="title"
        class="card-title"
      >
        {{ title }}
      </h1>
      <h2
        v-if="subTitle"
        class="card-sub-title"
      >
        {{ subTitle }}
      </h2>
      <slot />
    </div>
  </div>
</template>

<script>
export default {
  props: ['title', 'subTitle']
}
</script>

<style scoped>
.card-title {
  margin-bottom: 3.5rem;
}

.card-sub-title {
  margin-bottom: 2.5rem;
}
</style>
