<template>
  <div>
    <div
      v-if="error"
      class="alert alert-danger mb-4"
    >
      {{ error }}
    </div>

    <Spinner v-if="waiting" />
    <TransformationResult
      v-else-if="result"
      :transformation-result="result"
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
          @click.prevent="transform()"
        >
          <i class="material-icons middle md-20">redo</i> {{ $t('transform') }}
        </button>
      </div>
    </form>
  </div>
</template>

<script>
import Spinner from '@/components/vue/animations/Spinner'
import TransformationResult from './TransformationResult'
import { TRANSFORM_URL } from '@/urls'

export default {
  components: {
    Spinner,
    TransformationResult
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
   01 SOME-ITEM.
    05 ITEM-NAME PIC X(15) VALUE "Item Name".
    05 PRICE PIC 999V99 VALUE 99.99.
    05 AMOUNT PIC 999 VALUE 42.
   01 SOME-PERSON.
    05 PERSON-NAME PIC X(20) VALUE "Grace Hopper".
    05 PERSON-ADDRESS.
     10 CITY PIC X(15) VALUE "Cambridge".
     10 STATE PIC X(15) VALUE "Massachusetts".
   77 TOTAL-AMOUNT PIC 99999V99.
   77 DISCOUNT-BOUNDARY PIC 99999V99 VALUE 1000.00.
   77 DISCOUNT-PERCENT PIC 99 VALUE 10.
   77 DISCOUNT-AMOUNT PIC 99999V99.
 PROCEDURE DIVISION.
  BATCH-DISCOUNT.
   PERFORM COMPUTE-DISCOUNT.
   PERFORM DISPLAY-DISCOUNT.
   STOP RUN.
  COMPUTE-DISCOUNT.
   MULTIPLY AMOUNT BY PRICE GIVING TOTAL-AMOUNT.
   IF TOTAL-AMOUNT > DISCOUNT-BOUNDARY
    MULTIPLY TOTAL-AMOUNT BY DISCOUNT-PERCENT GIVING DISCOUNT-AMOUNT
    DIVIDE 100 INTO DISCOUNT-AMOUNT
    SUBTRACT DISCOUNT-AMOUNT FROM TOTAL-AMOUNT.
  DISPLAY-DISCOUNT.
   DISPLAY PERSON-NAME.
   DISPLAY "Total: ", TOTAL-AMOUNT.
   DISPLAY "Discount: ", DISCOUNT-AMOUNT.`,
        format: 'TANDEM'
      },
      result: null,
      waiting: false
    }
  },
  methods: {
    transform: function () {
      this.error = null
      this.waiting = true

      let xhr = new XMLHttpRequest()
      xhr.open('POST', TRANSFORM_URL, true)
      xhr.timeout = 60000
      xhr.setRequestHeader('Content-Type', 'application/json')

      xhr.onload = () => {
        if (xhr.readyState === 4) {
          if (xhr.status === 200) {
            this.result = xhr.response
          } else {
            this.error = xhr.responseText
          }

          this.waiting = false
        }
      }

      xhr.onerror = function() {
        this.error = xhr.responseText
        this.waiting = false
      }

      xhr.send(JSON.stringify(this.param))
    }
  },
  i18n: {
    messages: {
      en: {
        fixed: 'FIXED',
        line_format: 'Line format',
        tandem: 'TANDEM',
        transform: 'Transform',
        variable: 'VARIABLE'
      }
    }
  }
}
</script>

<style scoped>
.code {
  font-family: courier, "courier new", monospace;
  font-size: 0.8rem;
}
</style>
