<template>
  <footer class="footer bg-dark text-white">
    <div class="container">
      <div class="row">
        <div class="col-sm-4 col-xl-2 offset-xl-3">
          <h5>GitHub</h5>
          <ul>
            <li>
              <a href="https://github.com/uwol/proleap-cobol-parser">COBOL parser</a>
            </li>
            <li>
              <a href="https://github.com/uwol/proleap-vb6-parser">VB 6.0 parser</a>
            </li>
          </ul>
        </div>
        <div class="col-sm-4 col-xl-2">
          <h5>Blog</h5>
          <ul>
            <li>
              <router-link :to="{ name: 'blog-2018-01-02'}">
                Transformation pipeline
              </router-link>
            </li>
          </ul>
        </div>
        <div class="col-sm-4 col-xl-2">
          <h5>Contact</h5>
          <ul>
            <li>
              <a href="mailto:contact@proleap.io">contact@proleap.io</a>
            </li>
            <li>
              <a href="tel:+49 251 26538-136">+49 251 26538-136</a>
            </li>
          </ul>
        </div>
      </div>
      <div class="footer-imprint text-center">
        © {{ new Date().getFullYear() }} ProLeap.io, a <a href="http://www.nitroventures.de">nitro ventures</a> product
        &mdash;
        <router-link :to="{ name: 'imprint'}">
          Imprint
        </router-link>
        &mdash;
        <router-link :to="{ name: 'privacy'}">
          Privacy Policy
        </router-link>
      </div>
    </div>
  </footer>
</template>

<script>
export default {
}
</script>

<style scoped>
.footer {
  padding: 4rem 0;
}

.footer h4 {
  margin-bottom: 0.5rem;
}

.footer a {
  color: white;
}

.footer ul {
  list-style: none;
  padding-left: 0;
}

.footer ul li + li {
  margin-top: 0.5rem;
}

.footer .footer-imprint {
  margin-top: 4rem;
}

.footer .footer-imprint a {
  text-decoration: underline;
}
</style>
