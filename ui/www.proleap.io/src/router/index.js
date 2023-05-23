import { createRouter, createWebHistory } from 'vue-router'

import BlogArticles from '@/pages/blog/BlogArticles'
import BlogProLeapTransformationPipeline from '@/pages/blog/2018-01-02_uwol_ProLeap-transformation-pipeline'
import Contact from '@/pages/Contact'
import Home from '@/pages/Home'
import Imprint from '@/pages/Imprint'
import Privacy from '@/pages/Privacy'

/* !!! remember to update root/sitemap.xml; alphabetic order !!! */
const routes = [
  { path: '/', name: 'home', component: Home },
  { path: '/blog', name: 'blog', component: BlogArticles },
  { path: '/blog/proleap-transformation-pipeline', name: 'blog-2018-01-02', component: BlogProLeapTransformationPipeline },
  { path: '/contact', name: 'contact', component: Contact },
  { path: '/imprint', name: 'imprint', component: Imprint },
  { path: '/privacy', name: 'privacy', component: Privacy },
  { path: '/*', redirect: '/' }
]
/* !!! remember to update root/sitemap.xml; alphabetic order !!! */

const router = createRouter({
  history: createWebHistory(),
  routes: routes,
  // scroll to page top on routing
  scrollBehavior (to, from, savedPosition) {
    if (to && to.hash) {
      let id = to.hash.split('#').pop()
      return window.scroll({ top: document.getElementById(id).offsetTop + 200, behavior: 'smooth' })
    } else if (to) {
      return window.scroll({ top: to.offsetTop + 200, behavior: 'smooth' })
    } else if (savedPosition) {
      return savedPosition
    } else {
      return { x: 0, y: 0 }
    }
  }
})

export default router
