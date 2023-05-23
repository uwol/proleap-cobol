<template>
  <GridRow v-if="articles">
    <ArticleListItem
      v-for="article in articles"
      :key="article.key"
      :article="article"
    />
  </GridRow>
</template>

<script>
import ArticleListItem from './ArticleListItem'
import GridRow from '@/components/vue/grid/GridRow'

export default {
  components: {
    ArticleListItem,
    GridRow
  },
  props: ['articles'],
}
</script>
