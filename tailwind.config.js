// const { fontFamily } = require('tailwindcss/defaultTheme');

/** @type {import('tailwindcss').Config} */
module.exports = {
  darkMode: 'class',
  // darkMode: ['class', '[data-scheme="dark"]'],
  // darkMode: ['class', '[data-theme="dark"]'],
  content: ['./src/**/*.{js,ts,jsx,tsx}'],
  theme: {
    extend: {
      // fontFamily: {
      //   sans: ['var(--font-sans)', ...fontFamily.sans],
      // },
      colors: {
        'color-brand-primary': '#23D2B3',
        'color-brand-secondary': '',
        'color-brand-tertiary': '',
        'color-text-primary': '#FFFFFF',
        'color-text-secondary': '#EAEAEA',
        'color-text-inverted': '',
        'color-text-link': '',
        'color-text-ok': '',
        'color-text-warn': '',
        'color-text-danger': '',
        'color-background-primary': '#060A0D',
        'color-background-secondary': '#101417',
        'color-background-tertiary': '#1B1D1F',
        'color-ui-primary': '',
        'color-ui-secondary': '',
        'color-ui-tertiary': '',
        'color-ui-ok': '',
        'color-ui-warn': '',
        'color-ui-danger': '',
      },

      keyframes: {
        'accordion-down': {
          from: { height: 0 },
          to: { height: 'var(--radix-accordion-content-height)' },
        },
        'accordion-up': {
          from: { height: 'var(--radix-accordion-content-height)' },
          to: { height: 0 },
        },
      },
      animation: {
        'accordion-down': 'accordion-down 0.2s ease-out',
        'accordion-up': 'accordion-up 0.2s ease-out',
      },
    },
    screens: {
      sm: '640px',
      // => @media (min-width: 640px) { ... }

      md: '768px',
      // => @media (min-width: 768px) { ... }

      lg: '1024px',
      // => @media (min-width: 1024px) { ... }

      xl: '1280px',
      // => @media (min-width: 1280px) { ... }

      '2xl': '1536px',
      // => @media (min-width: 1536px) { ... }
    },
  },
  plugins: [require('tailwindcss-animate')],
};
