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
      //   sans: ['var(font-sans)', ...fontFamily.sans],
      // },
      colors: {
        // 'color-brand-primary': '#23D2B3',
        // 'color-brand-secondary': '',
        // 'color-brand-tertiary': '',
        // 'color-text-primary': '#FFFFFF',
        // 'color-text-secondary': '#EAEAEA',
        // 'color-text-inverted': '',
        // 'color-text-link': '',
        // 'color-text-ok': '',
        // 'color-text-warn': '',
        // 'color-text-danger': '',
        // 'color-background-primary': '#060A0D',
        // 'color-background-secondary': '#101417',
        // 'color-background-tertiary': '#1B1D1F',
        // 'color-ui-primary': '',
        // 'color-ui-secondary': '',
        // 'color-ui-tertiary': '',
        // 'color-ui-ok': '',
        // 'color-ui-warn': '',
        // 'color-ui-danger': '',

        // admin
        white: '#FFFFFF',
        black: '#000000',
        grey: '#bbbdc1',
        background: '240 10% 3.9%',
        foreground: '0 0% 98%',
        card: '240 10% 3.9%',
        'card-foreground': '0 0% 98%',
        popover: '240 10% 3.9%',
        'popover-foreground': '0 0% 98%',
        primary: '0 0% 98%',
        'primary-foreground': '240 5.9% 10%',
        secondary: '240 3.7% 15.9%',
        'secondary-foreground': '0 0% 98%',
        muted: '240 3.7% 15.9%',
        'muted-foreground': '240 5% 64.9%',
        accent: '240 3.7% 15.9%',
        'accent-foreground': '0 0% 98%',
        destructive: '0 62.8% 30.6%',
        'destructive-foreground': '0 0% 98%',
        border: '240 3.7% 15.9%',
        input: '240 3.7% 15.9%',
        ring: '240 4.9% 83.9%',

        primary: {
          100: '#38393d',
          500: '#212327',
          800: '#090b0e',
        },
        secondary: {
          100: '#394b4a',
          500: '#78bfac',
          800: '#4efec9',
        },
      },

      keyframes: {
        'accordion-down': {
          from: { height: 0 },
          to: { height: 'var(radix-accordion-content-height)' },
        },
        'accordion-up': {
          from: { height: 'var(radix-accordion-content-height)' },
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
