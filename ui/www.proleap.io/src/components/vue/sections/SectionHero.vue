<template>
  <div class="section--hero">
    <slot />
  </div>
</template>

<style scoped>
.section--hero {
  display: flex;
  align-items: center;
  padding: 5em 0;
  min-height: 8rem;
}

.section--hero h1 {
  margin-bottom: 7.5rem;
}

.section--hero .hero--bg {
  height: 20rem;
  margin-left: 0.5em;
  margin-right: 0.5em;
}

.section--hero p a {
  color: inherit;
}

.section--hero p a:hover {
  color: inherit;
}
</style>
