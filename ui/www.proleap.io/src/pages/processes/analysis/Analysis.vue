<template>
  <div>
    <div
      v-if="error"
      class="alert alert-danger mb-4"
    >
      {{ error }}
    </div>

    <Spinner v-if="waiting" />
    <AnalysisResult
      v-else-if="result"
      :analysis-result="result"
    />
    <form v-else>
      <div class="form-group">
        <label for="line_format">{{ $t('line_format') }}</label>
        <select
          v-model="param.format"
          class="form-control"
        >
          <option value="TANDEM">
            {{ $t('tandem') }}
          </option>
          <option value="FIXED">
            {{ $t('fixed') }}
          </option>
          <option value="VARIABLE">
            {{ $t('variable') }}
          </option>
        </select>
      </div>
      <div class="form-group mb-3">
        <textarea
          v-model="param.code"
          class="form-control code"
          rows="10"
        />
      </div>
      <div class="form-group">
        <button
          class="btn btn-primary shadow mr-2"
          @click.prevent="analyze()"
        >
          <i class="material-icons middle md-20">search</i> {{ $t('analyze') }}
        </button>
      </div>
    </form>
  </div>
</template>

<script>
import AnalysisResult from './AnalysisResult'
import Spinner from '@/components/vue/animations/Spinner'
import { ANALYZE_URL } from '@/urls'

export default {
  components: {
    AnalysisResult,
    Spinner
  },
  data () {
    return {
      error: null,
      param: {
        code:
` IDENTIFICATION DIVISION.
  PROGRAM-ID. EXAMPLE.
 DATA DIVISION.
  WORKING-STORAGE SECTION.
   01 INVALIDITEM1 PIC 99 VALUE "test".
   01 INVALIDITEM2 PIC X(5) VALUE 42.
   01 VALIDITEM1 PIC 99.9 VALUE 42.
   01 VALIDITEM2 PIC 99 VALUE 42.
   01 VALIDITEM3 PIC X(5) VALUE "test".
   01 VALIDITEM4 PIC 99 VALUE ZERO.
 PROCEDURE DIVISION.
  INIT.
   PERFORM PROC1 THROUGH PROC3.
   STOP RUN.
  PROC1.
   PERFORM PROC2.
  PROC2.
   PERFORM PROC4.
  PROC3.
   Display "Proc3".
  PROC4.
   PERFORM PROC1.`,
        format: 'TANDEM'
      },
      result: null,
      waiting: false
    }
  },
  methods: {
    analyze: function () {
      this.error = null
      this.waiting = true

      let xhr = new XMLHttpRequest()
      xhr.open('POST', ANALYZE_URL, true)
      xhr.timeout = 60000
      xhr.setRequestHeader('Content-Type', 'application/json')

      xhr.onload = () => {
        if (xhr.readyState === 4) {
          if (xhr.status === 200) {
            this.result = JSON.parse(xhr.response)
          } else {
            this.error = xhr.responseText
          }

          this.waiting = false
        }
      }

      xhr.onerror = () => {
        this.error = xhr.responseText
        this.waiting = false
      }

      xhr.send(JSON.stringify(this.param))
    }
  },
  i18n: {
    messages: {
      en: {
        analyze: 'Analyze',
        fixed: 'FIXED',
        line_format: 'Line format',
        tandem: 'TANDEM',
        variable: 'VARIABLE'
      }
    }
  }
}
</script>

<style scoped>
.code {
  font-family: monospace;
  font-size: 0.8rem;
}
</style>
