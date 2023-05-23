<template>
  <div>
    <SectionHero class="bg-dark text-white">
      <div class="container">
        <GridRow class="mb-0">
          <GridCell class="col-12 col-md-8 mb-0">
            <h1 class="animated fadeIn mb-5">
              {{ $t('run_cobol_or_transform') }}
            </h1>
            <p class="animated fadeInUp mb-3">
              {{ $t('run_cobol_or_transform_description') }}
            </p>
          </GridCell>
        </GridRow>
      </div>
    </SectionHero>

    <Section>
      <div class="container">
        <GridRow>
          <GridCell class="col-12 offset-md-1 col-md-10 offset-xl-2 col-xl-8">
            <H2Box :title="$t('analysis_title')" />
            <p class="mb-5">
              {{ $t('analysis_description') }}
            </p>
            <Analysis />
          </GridCell>
        </GridRow>
      </div>
    </Section>

    <Section spacer="true">
      <div class="container">
        <GridRow>
          <GridCell class="col-12 offset-md-1 col-md-10 offset-xl-2 col-xl-8">
            <H2Box :title="$t('execution_title')" />
            <p class="mb-5">
              {{ $t('execution_description') }}
            </p>
            <Execution />
          </GridCell>
        </GridRow>
      </div>
    </Section>

    <Section>
      <div class="container">
        <GridRow>
          <GridCell class="col-12 offset-md-1 col-md-10 offset-xl-2 col-xl-8">
            <H2Box :title="$t('transform_title')" />
            <p class="mb-5">
              {{ $t('transform_description') }}
            </p>
            <Transformation />
          </GridCell>
        </GridRow>
      </div>
    </Section>

    <Section spacer="true">
      <div class="container">
        <H2Box :title="$t('about_us')" />
        <GridRow>
          <GridCell class="col-12 offset-md-2 col-md-8">
            <p>
              {{ $t('about_us_description') }}
            </p>
            <p>
              {{ $t('about_us_open_source') }}
            </p>
          </GridCell>
        </GridRow>
      </div>
    </Section>
  </div>
</template>

<script>
import Analysis from '@/pages/processes/analysis/Analysis'
import Execution from '@/pages/processes/execution/Execution'
import GridCell from '@/components/vue/grid/GridCell'
import GridRow from '@/components/vue/grid/GridRow'
import H2Box from '@/components/vue/heading/H2Box'
import Section from '@/components/vue/sections/Section'
import SectionHero from '@/components/vue/sections/SectionHero'
import Transformation from '@/pages/processes/transformation/Transformation'

export default {
  components: {
    Analysis,
    Execution,
    GridCell,
    GridRow,
    H2Box,
    Section,
    SectionHero,
    Transformation
  },
  data () {
    return {
      issuesImgUrl: '/img/pages/index/hero.png',
      analyticsImgUrl: '/img/pages/index/proleap-analytics.png'
    }
  },
  i18n: {
    messages: {
      en: {
        about_us: 'About us',
        about_us_description: 'We want to help enterprises that rely on legacy COBOL technology to take the step into the cloud today. Set free resources to accelerate your business instead of spending tons of money and time for running COBOL in your own infastrcuture.',
        about_us_open_source: 'Devs, start working with our open-source components. We invite you to use and expand our COBOL parser. Check it out and fork it on GitHub.',
        analysis_title: 'Analyze your COBOL code',
        analysis_description: 'Get a quick and transparent overview of your COBOL code. Navigate through the code like you would expect from a modern development platform. This is a good starting point for COBOL optimization and preparation for transforming to a different platform.',
        execution_title: 'Execute COBOL in the cloud now',
        execution_description: 'Taking COBOL to the cloud might sound a bold promise. We hear you, so try it out directly in your browser. Now.',
        run_cobol_or_transform: 'Run COBOL in the cloud or transform it to JAVA',
        run_cobol_or_transform_description: 'ProLeap is a cloud-platform that empowers enterprises to run COBOL programs in the cloud. Secure, low-cost and fast.',
        run_sample: 'Run sample file',
        transform_title: 'Transform your COBOL code to JAVA',
        transform_description: 'You can move your COBOL to executable JAVA with our transformation components. This will accelerate your COBOL modernization project by a factor of 10.',
      }
    }
  }
}
</script>
