<template>
  <div>
    <table 
      v-if="issues && issues.length" 
      class="table table-striped table-hover table-sm issues-table w-100"
    >
      <tbody>
        <tr 
          v-for="(issue, index) in issues" 
          :key="index"
        >
          <td>
            <a @click.prevent="showIssueCodeView(issue.href)">
              {{ issue.description }}
            </a>
          </td>
          <td>
            <span :class="determineSeverityClass(severities[issue.severity])">
              {{ severities[issue.severity] }}
            </span>
          </td>
        </tr>
      </tbody>
    </table>
    <p 
      v-else 
      class="text-center pt-3 mx-5 px-5"
    >
      Great, no issues were found in your code. If you want us to add an issue pattern, please let us know.
    </p>
  </div>
</template>

<script>
import $ from 'jquery'

export default {
  props: ['issues'],
  data () {
    return {
      severities: ['info', 'minor', 'major', 'critical', 'blocker']
    }
  },
  methods: {
    determineSeverityClass: function (severity) {
      return `badge badge-secondary badge-severity-${severity}`;
    },
    showIssueCodeView: function (href) {
      $('.nav-tabs a[href="#tab-code"]').tab('show')

      let id = href.split('#').pop()
      this.$router.push({ hash: id })
    }
  },
  i18n: {
    messages: {
      en: {
        context: 'Context',
        description: 'Description',
        severity: 'Severity',
      }
    }
  }
}
</script>

<style scoped>
.badge-severity-info {
  background-color: #2e7d32
}

.badge-severity-minor {
  background-color: #f9a825
}

.badge-severity-major {
  background-color: #ef6c00
}

.badge-severity-critical {
  background-color: #c62828
}

.badge-severity-blocker {
  background-color: #6a1b9a
}
</style>
