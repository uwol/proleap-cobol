<template>
  <div class="code-analysis">
    <!-- eslint-disable vue/no-v-html -->
    <pre
      class="code border rounded p-2 bg-white"
      v-html="codeView"
    />
    <!--eslint-enable-->
  </div>
</template>

<script>
import $ from 'jquery'

export default {
  props: ['codeView'],
  mounted: function () {
    let _self = this

    $('[data-hrefs]').popover({
      animation: false,
      html: true,
      trigger: 'click hover',
      placement: 'top',
      title: function () {
        let className = $(this).attr('class')
        return className
      },
      content: function () {
        let hrefs = $(this).attr('data-hrefs')
        let callers = $(this).attr('data-callers')
        return _self.renderContent(hrefs, callers)
      }
    })

    $('body').on('keyup', () => {
      this.closePopovers()
    })

    $('.code-analysis .code a[href*="#"]').on('click touchstart', function (event) {
      _self.jumpIntoModule(event, this)
    })
  },
  methods: {
    bindJumpIntoModule: function (element) {
      let _self = this

      $(element).on('click touchstart', function (event) {
        _self.jumpIntoModule(event, this)
      })
    },
    bindPopoversClose: function (element) {
      $(element).on('click touchstart', () => {
        this.closePopovers()
      })
    },
    closePopovers: function () {
      // twice due to bootstrap bug, otherwise popovers are not closed
      $('[data-hrefs]').popover('hide')
      $('[data-hrefs]').popover('hide')
    },
    jumpIntoModule: function (event, element) {
      event.preventDefault()

      let id = $(element).attr('href').split('#').pop()
      this.$router.push({ hash: id })

      this.closePopovers()
    },
    renderContent: function (hrefs, callers) {
      var ids = hrefs.split(",").filter(Boolean)
      var callersArray = callers.split(",").filter(Boolean)
      var result = $('<div/>')

      var numberOfCallsText = this.renderNumberOfCallsText(ids)
      result.text(numberOfCallsText)

      var ul = $('<ul/>')
      result.append(ul)

      for (let i = 0; i < ids.length && i < callersArray.length; i++) {
        var id = ids[i]
        var caller = callersArray[i]

        var li = $('<li/>')
        var link = $('<a/>').attr('href', id).text(caller)

        li.append(link)
        ul.append(li).append(" ")

        this.bindPopoversClose(link)
        this.bindJumpIntoModule(link)
      }

      return result
    },
    renderNumberOfCallsText: function (ids) {
      var callsNumber = ids.length
      var callsText

      if (callsNumber === 0){
        callsText = 'no calls'
      } else if (callsNumber === 1){
        callsText = '1 call from:'
      } else {
        callsText = callsNumber + ' calls from:'
      }

      return callsText
    }
  },
  i18n: {
    messages: {
      en: {
        code: 'Code'
      }
    }
  }
}
</script>

<style>
pre.code {
  font-family: courier, "courier new", monospace;
  font-weight: 600;
  overflow: auto;
  white-space: pre;
  word-wrap: normal;
}

pre.code div {
  display: inline;
}

pre.code a {
  color: #0d47a1;
}

pre.code x-c,
pre.code x-i,
pre.code x-l {
  font-weight: 400;
}

.popover .popover-title {
  font-size: 12px;
}

.popover .popover-content {
  font-size: 12px;
}

.popover .popover-content ul {
  margin-bottom: 0;
  padding-left: 3em;
}
</style>
