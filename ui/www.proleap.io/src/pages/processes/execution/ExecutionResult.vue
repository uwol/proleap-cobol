<template>
  <div>
    <div v-if="executionResult.output">
      <H3Box :title="$t('output')" />
      <Code :code="outputText" />
    </div>

    <div v-if="executionResult.values">
      <H3Box :title="$t('state')" />
      <div class="border rounded p-2 bg-white">
        <table class="w-100">
          <tbody>
            <tr 
              v-for="(item, index) in executionResult.values" 
              :key="index"
            >
              <td class="key">
                {{ item.key }}
              </td>
              <td class="value">
                {{ item.value }}
              </td>
            </tr>
          </tbody>
        </table>
      </div>
    </div>
  </div>
</template>

<script>
import Code from '@/components/app/code/Code'
import H3Box from '@/components/vue/heading/H3Box'

export default {
  components: {
    Code, H3Box
  },
  props: ['executionResult'],
  computed: {
    outputText: function () {
      return this.executionResult.output.join('\n')
    }
  },
  i18n: {
    messages: {
      en: {
        output: 'Output',
        state: 'Final state'
      }
    }
  }
}
</script>

<style scoped>
.key {
  font-size: 0.8rem;
}

.value {
  font-family: courier, "courier new", monospace;
  font-size: 0.8rem;
  white-space: pre-wrap;
}
</style>
