<template>
  <nav class="navbar fixed-top navbar-expand-md navbar-light bg-white border-bottom">
    <div class="container">
      <router-link 
        :to="{ name: 'home'}" 
        class="navbar-brand" 
        alt="Proleap Platform" 
        title="ProLeap COBOL Transformation Platform"
      >
        <img 
          src="./logo.png" 
          class="hidden-md-down d-inline mr-2" 
          alt="ProLeap Logo" 
          title="ProLeap Logo"
        >
        <span class="d-none d-sm-inline">ProLeap</span>
      </router-link>

      <button 
        class="navbar-toggler" 
        type="button" 
        data-toggle="collapse" 
        data-target="#navbar-collapse" 
        aria-controls="navbar-collapse" 
        aria-expanded="false" 
        aria-label="Toggle navigation"
      >
        <span class="navbar-toggler-icon" />
      </button>

      <div
        id="navbar-collapse" 
        class="collapse navbar-collapse justify-content-end text-center"
      >
        <ul class="navbar-nav">
          <li 
            class="nav-item" 
            data-toggle="collapse" 
            data-target=".navbar-collapse.show"
          >
            <router-link :to="{ name: 'blog' }">
              <span :class="['nav-link', currentRoute === 'blog' || currentRoute === 'blog-2018-01-02' ? 'active' : '']">Blog</span>
            </router-link>
          </li>

          <li 
            class="nav-item" 
            data-toggle="collapse" 
            data-target=".navbar-collapse.show"
          >
            <router-link :to="{ name: 'contact' }">
              <span :class="['nav-link', currentRoute === 'contact' ? 'active' : '']">Contact</span>
            </router-link>
          </li>

          <li 
            class="nav-item" 
            data-toggle="collapse" 
            data-target=".navbar-collapse.show"
          >
            <a
              href="https://github.com/proleap/proleap-cobol"
              target="_blank"
            >
              <span class="nav-link btn btn-primary btn-sm">
                GitHub
              </span>
            </a>
          </li>
        </ul>
      </div>
    </div>
  </nav>
</template>

<script>
export default {
  data () {
    return {
      currentRoute: ''
    }
  },
  watch: {
    $route (to) {
      this.currentRoute = to.matched[0].name
    }
  }
}
</script>

<style scoped>
.navbar {
  font-weight: var(--navbar-font-weight);
}

.navbar .navbar-brand {
  color: var(--text-color);
}

.navbar .navbar-brand img {
  width: 40px;
  height: 40px;
}

.navbar .navbar-toggler {
  border-color: transparent;
}

.navbar .navbar-collapse .nav-link {
  color: var(--text-color);
}

.navbar .navbar-collapse .nav-link.active {
  color: var(--text-color);
}

.navbar .navbar-collapse .dropdown-menu {
  text-align: center;
}

@media (min-width: 768px) {
  .navbar .navbar-collapse .nav-link {
    margin-left: .5rem;
    margin-right: .5rem;
    border-bottom: 1px solid transparent;
  }

  .navbar .navbar-collapse .nav-link.active {
    border-bottom: 1px solid var(--text-color);
  }

  .navbar .navbar-collapse .dropdown-menu {
    text-align: left;
  }

  .navbar.navbar-middle .navbar-collapse .dropdown-menu .nav-link {
    margin-left: .5rem;
    margin-right: .5rem;
  }
}

.navbar .nav-link,
.navbar .navbar-brand {
  font-family: var(--header-font-family);
}
</style>
