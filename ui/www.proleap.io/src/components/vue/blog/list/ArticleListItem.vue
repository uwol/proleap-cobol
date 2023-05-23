<template>
  <GridCell class="col-md-6 col-xl-4">
    <Scale>
      <Card>
        <article>
          <div class="mb-2 text-muted">
            <span class="mr-2">{{ article.readTime }} min read</span>
            <span class="mr-2">|</span>
            <span>{{ article.dateString }}</span>
          </div>
          <div class="mb-3">
            <h2 class="header">
              <router-link :to="{ name: article.key }">
                {{ article.title }}
              </router-link>
            </h2>
            <p>
              <router-link :to="{ name: article.key }">
                {{ article.previewText }}
              </router-link>
            </p>
            <div class="author-box mt-5">
              <div class="author-img-box">
                <img
                  :src="getAuthorImage(article.authorKey)"
                  :alt="article.authorName"
                  class="rounded-circle blog-author-img mr-1 mb-2"
                >
              </div>
              <div class="blog-author-box">
                <div class="blog-author-name-box">
                  <span>{{ article.authorName }}</span>
                </div>
                <div class="blog-header-author-title-box">
                  {{ article.authorTitle }}
                </div>
              </div>
            </div>
          </div>
        </article>
      </Card>
    </Scale>
  </GridCell>
</template>

<script>
import Card from '@/components/vue/cards/Card'
import GridCell from '@/components/vue/grid/GridCell'
import Scale from '@/components/vue/animations/Scale'

export default {
  components: {
    Card, GridCell, Scale
  },
  props: [
    'article'
  ],
  methods: {
    getAuthorImage: function (authorKey) {
      return `/img/pages/team/${authorKey}.jpg`
    }
  }
}
</script>

<style scoped>
.header {
  font-size: 1.5rem;
  margin-bottom: 1.5rem;
}

.author-box {
  display: flex;
}

.blog-author-img {
  width: 50px;
  height: 50px;
  display: inline-block;
  vertical-align: middle;
}

.blog-author-box {
  flex: 1 1 auto;
  padding-left: 15px;
}

.blog-author-box .blog-author-name-box {
  font-weight: 600;
}

.blog-author-box .blog-author-name-box a {
  text-decoration: none;
  color: #212121;
  cursor: pointer;
}

a {
  text-decoration: none;
  color: #212121;
  cursor: pointer;
}

a:hover {
  text-decoration: none;
}
</style>
