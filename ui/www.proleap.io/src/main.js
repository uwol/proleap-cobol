/*
* import libraries
*/
import { createApp } from 'vue'
import { createI18n } from 'vue-i18n'
import locale2 from 'locale2'

import App from './App.vue'
import router from './router'
import { LOCALE_FALLBACK } from '@/params'

/*
* import JS
*/
import 'bootstrap'

/*
* import CSS
*/
import 'bootstrap/dist/css/bootstrap.min.css'
import 'animate.css/animate.min.css'
import '@/assets/fonts/roboto/css/roboto.css'
import '@/assets/fonts/source-sans-pro/css/source-sans-pro.css'
import '@/assets/fonts/material-icons/css/material-icons.css'

import '@/assets/css/blog.css'
import '@/assets/css/bootstrap-override.css'
import '@/assets/css/colors.css'
import '@/assets/css/typography.css'
import '@/assets/css/variables.css'


const lang = locale2.substring(0, 2)
const i18n = createI18n({ locale: lang, fallbackLocale: LOCALE_FALLBACK, silentFallbackWarn: true, silentTranslationWarn: true })

const app = createApp(App)

app.use(i18n)
app.use(router)
// DO NOT add any other VueJS libraries globally; most smaller vuejs libraries break with each VueJS major version upgrade, they will block the VueJS upgrade path

app.mount('#app')
