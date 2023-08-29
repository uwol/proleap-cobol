<template>
  <Section>
    <div class="container">
      <div class="row">
        <div class="col-12 col-md-2">
          <aside class="blog-share-widget d-none d-md-block">
            <ul>
              <li>
                <a :href="getLinkedInShareUrl(article)">
                  <svg viewBox="0 0 24 24">
                    <path
                      fill="#212529"
                      d="M19,3A2,2 0 0,1 21,5V19A2,2 0 0,1 19,21H5A2,2 0 0,1 3,19V5A2,2 0 0,1 5,3H19M18.5,18.5V13.2A3.26,3.26 0 0,0 15.24,9.94C14.39,9.94 13.4,10.46 12.92,11.24V10.13H10.13V18.5H12.92V13.57C12.92,12.8 13.54,12.17 14.31,12.17A1.4,1.4 0 0,1 15.71,13.57V18.5H18.5M6.88,8.56A1.68,1.68 0 0,0 8.56,6.88C8.56,5.95 7.81,5.19 6.88,5.19A1.69,1.69 0 0,0 5.19,6.88C5.19,7.81 5.95,8.56 6.88,8.56M8.27,18.5V10.13H5.5V18.5H8.27Z"
                    />
                  </svg>
                </a>
              </li>
              <li>
                <a :href="getTwitterShareUrl(article)">
                  <svg viewBox="0 0 24 24">
                    <path
                      fill="#212529"
                      d="M17.71,9.33C18.19,8.93 18.75,8.45 19,7.92C18.59,8.13 18.1,8.26 17.56,8.33C18.06,7.97 18.47,7.5 18.68,6.86C18.16,7.14 17.63,7.38 16.97,7.5C15.42,5.63 11.71,7.15 12.37,9.95C9.76,9.79 8.17,8.61 6.85,7.16C6.1,8.38 6.75,10.23 7.64,10.74C7.18,10.71 6.83,10.57 6.5,10.41C6.54,11.95 7.39,12.69 8.58,13.09C8.22,13.16 7.82,13.18 7.44,13.12C7.81,14.19 8.58,14.86 9.9,15C9,15.76 7.34,16.29 6,16.08C7.15,16.81 8.46,17.39 10.28,17.31C14.69,17.11 17.64,13.95 17.71,9.33M12,2A10,10 0 0,1 22,12A10,10 0 0,1 12,22A10,10 0 0,1 2,12A10,10 0 0,1 12,2Z"
                    />
                  </svg>
                </a>
              </li>
            </ul>
          </aside>
        </div>
        <div class="col-12 col-md-8">
          <article>
            <div class="blog-header">
              <div class="blog-header-img-box">
                <img
                  :src="getAuthorImage(article.authorKey)"
                  class="rounded-circle blog-author-img"
                  alt="Author"
                >
              </div>
              <div class="blog-header-author-box">
                <div class="blog-header-author-name-box">
                  <span>{{ article.authorName }}</span>
                </div>
                <div class="blog-header-author-title-box">
                  {{ article.authorTitle }}
                </div>
                <div class="mb-2 text-muted">
                  <span class="mr-2">{{ article.readTime }} min read</span>
                  <span class="mr-2">|</span>
                  <span>{{ article.dateString }}</span>
                </div>
              </div>
            </div>

            <div class="blog-content">
              <slot />
            </div>
          </article>
        </div>
      </div>
    </div>
  </Section>
</template>

<script>
import Section from '@/components/vue/sections/Section'

export default {
  components: {
    Section
  },
  props: [
    'article'
  ],
  methods: {
    getAuthorImage: function (authorKey) {
      return `/img/pages/team/${authorKey}.jpg`
    },
    getLinkedInShareUrl: function (article) {
      let url = encodeURIComponent(`https://www.proleap.io${article.path}`)
      let title = encodeURI(article.title)
      return `http://www.linkedin.com/shareArticle?url=${url}&title=${title}`
    },
    getTwitterShareUrl: function (article) {
      let url = encodeURIComponent(`https://www.proleap.io${article.path}`)
      let text = encodeURI(article.title)
      return `https://twitter.com/share?url=${url}&text=${text}&hashtags=ProLeap`
    }
  }
}
</script>

<style scoped>
.blog-share-widget {
  position: fixed;
  top: 205px;
}

.blog-share-widget ul {
  list-style: none;
  padding-left: 0;
  margin-left: 0;
}

.blog-share-widget ul li svg {
  height: 1.5rem;
  margin-bottom: 0.5rem;
}

.blog-header {
  display: flex;
}

.blog-header .blog-header-img-box {
  flex: 0 0 auto;
}

.blog-header .blog-author-img {
  width: 75px;
  height: 75px;
  display: inline-block;
  vertical-align: middle;
}

.blog-header .blog-header-author-box {
  flex: 1 1 auto;
  padding-left: 15px;
}

.blog-header .blog-header-author-name-box {
  font-weight: 600;
}

.blog-header .blog-header-author-name-box  a {
  text-decoration: none;
  color: #212121;
  cursor: pointer;
}
</style>
