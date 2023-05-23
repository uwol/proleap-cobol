<template>
  <Section>
    <div class="container">
      <div class="row">
        <div class="col-12 offset-md-2 col-md-8">
          <h1 class="text-center animated fadeIn">
            ProLeap Blog
          </h1>
        </div>
      </div>
      <ArticlesList :articles="articles" />
    </div>
  </Section>
</template>

<script>
import ArticlesList from '@/components/vue/blog/list/ArticlesList'
import Section from '@/components/vue/sections/Section'
import { ARTICLES } from '@/config/blog'

export default {
  components: {
    ArticlesList,
    Section
  },
  data () {
    return {
      articles: ARTICLES
    }
  }
}
</script>
