<template>
  <Code :code="transformationResult" />
</template>

<script>
import Code from '@/components/app/code/Code'

export default {
  components: {
    Code
  },
  props: ['transformationResult'],
}
</script>
