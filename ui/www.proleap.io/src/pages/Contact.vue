<template>
  <Section>
    <div class="container">
      <H1Box title="Contact Us" />
      <div class="row">
        <div class="col-12 offset-md-2 col-md-8">
          <form @submit.prevent="sendContactFormRequest">
            <AlertSuccess
              v-if="messageSuccess"
              text="Thank you, your message has been sent."
            />
            <AlertDanger
              v-else-if="messageSuccess === false"
              text="Sorry, your message could not be sent."
            />

            <div
              v-if="showSpinner"
              class="text-center mb-4"
            >
              <div
                class="spinner-border"
                role="status"
              >
                <span class="sr-only">Loading...</span>
              </div>
            </div>

            <div class="form-group">
              <input
                v-model="name"
                type="text"
                name="name"
                aria-label="name"
                placeholder="Your name"
                class="form-control"
                required
              >
            </div>
            <div class="form-group">
              <input
                v-model="email"
                type="email"
                name="email"
                aria-label="email"
                placeholder="Email address"
                class="form-control"
                required
              >
            </div>
            <div class="form-group">
              <input
                v-model="phone"
                type="phone"
                name="phone"
                aria-label="phone"
                placeholder="Phone number"
                class="form-control"
                required
              >
            </div>
            <div class="form-group">
              <textarea
                v-model="message"
                name="message"
                aria-label="message"
                placeholder="Message"
                class="form-control"
                rows="5"
                required
              />
            </div>
            <input
              type="submit"
              value="Send message"
              class="btn btn-primary shadow"
            >
          </form>
        </div>
      </div>
    </div>
  </Section>
</template>

<script>
import AlertDanger from '@/components/vue/alerts/AlertDanger'
import AlertSuccess from '@/components/vue/alerts/AlertSuccess'
import H1Box from '@/components/vue/heading/H1Box'
import Section from '@/components/vue/sections/Section'
import { sendMail } from '@/service/mail-service'

export default {
  components: {
    AlertDanger,
    AlertSuccess,
    H1Box,
    Section
  },
  data () {
    return {
      email: '',
      messageSuccess: null,
      message: '',
      name: '',
      phone: '',
      showSpinner: false
    }
  },
  methods: {
    sendContactFormRequest: async function () {
      this.showSpinner = true

      sendMail(this.name, this.email, this.phone, this.message, (success) => {
        this.messageSuccess = success
        this.showSpinner = false
      })
    }
  }
}
</script>
