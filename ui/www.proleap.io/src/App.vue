<template>
  <div
    id="app"
    data-app
  >
    <Navigation />
    <Main>
      <router-view />
    </Main>
    <Footer />
  </div>
</template>

<script>
import Footer from '@/layouts/Footer'
import Main from '@/layouts/Main'
import Navigation from '@/layouts/Navigation'

export default {
  name: 'App',
  components: {
    Footer,
    Main,
    Navigation
  }
}
</script>
