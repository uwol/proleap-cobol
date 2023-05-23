<template>
  <div>
    <ul 
      class="nav nav-tabs mb-4" 
      role="tablist"
    >
      <li class="nav-item">
        <a 
          class="nav-link active" 
          data-toggle="tab" 
          href="#tab-issues" 
          role="tab"
        >{{ $t('issues') }}</a>
      </li>

      <li class="nav-item">
        <a 
          class="nav-link" 
          data-toggle="tab" 
          href="#tab-code" 
          role="tab"
        >{{ $t('code') }}</a>
      </li>
    </ul>

    <div class="tab-content">
      <div 
        id="tab-issues" 
        class="tab-pane active" 
        role="tabpanel"
      >
        <Issues :issues="analysisResult.issues" />
      </div>

      <div 
        id="tab-code" 
        class="tab-pane" 
        role="tabpanel"
      >
        <CodeView :code-view="analysisResult.codeView" />
      </div>
    </div>
  </div>
</template>

<script>
import CodeView from './tabs/CodeView'
import Issues from './tabs/Issues'

export default {
  components: {
    CodeView,
    Issues
  },
  props: ['analysisResult'],
  i18n: {
    messages: {
      en: {
        analysis_result: 'Analysis result',
        code: 'Code',
        issues: 'Issues',
      }
    }
  }
}
</script>
