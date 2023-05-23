import { MAIL_URL } from '@/urls'

export async function sendMail (name, email, phone, message, handler) {
  let xhr = new XMLHttpRequest()

  xhr.onload = function() {
    if (xhr.readyState === 4) {
      if (xhr.status === 200) {
        handler(true)
      } else {
        handler(false)
      }
    }
  }

  xhr.onerror = function() {
    handler(false)
  }

  xhr.open('POST', MAIL_URL, true)
  xhr.setRequestHeader('Content-Type', 'application/json')
  xhr.send(JSON.stringify({name: name, email: email, phone: phone, message: message}))
}
