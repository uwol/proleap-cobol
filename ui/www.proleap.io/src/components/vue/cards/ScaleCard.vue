<template>
  <Scale>
    <div class="card mb-3 p-2">
      <div class="card-body">
        <img
          v-if="imageUrl"
          :src="imageUrl"
          class="card-img-top pt-3 pb-3"
          alt=""
        >
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
  </Scale>
</template>

<script>
import Scale from '@/components/vue/animations/Scale'

export default {
  components: {
    Scale
  },
  props: ['imageUrl', 'title', 'subTitle'],
}
</script>

<style scoped>
.card {
  border-width: 0;
}

.card-img-top {
  max-height: 100px;
}

.card-title {
  margin-bottom: 3.5rem;
}

.card-sub-title {
  margin-bottom: 2.5rem;
}
</style>
