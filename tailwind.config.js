/** @type {import('tailwindcss').Config} */
module.exports = {
  darkMode: 'class',
  content: ['./src/**/*.{js,ts,jsx,tsx}'],
  theme: {
    extend: {
      fontFamily: {
        primary: 'Proxima Nova',
      },
      colors: {
        // admin
        white: '#FFFFFF',
        black: '#000000',
        grey: '#bbbdc1',
        teal: '#23D2B3',
        card:'#101417',
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

        border: 'hsl(var(--border))',
        input: 'hsl(var(--input))',
        ring: 'hsl(var(--ring))',
        background: 'hsl(var(--background))',
        foreground: 'hsl(var(--foreground))',
        primary: {
          DEFAULT: 'hsl(var(--primary))',
          foreground: 'hsl(var(--primary-foreground))',
        },
        secondary: {
          DEFAULT: 'hsl(var(--secondary))',
          foreground: 'hsl(var(--secondary-foreground))',
        },
        destructive: {
          DEFAULT: 'hsl(var(--destructive))',
          foreground: 'hsl(var(--destructive-foreground))',
        },
        muted: {
          DEFAULT: 'hsl(var(--muted))',
          foreground: 'hsl(var(--muted-foreground))',
        },
        accent: {
          DEFAULT: 'hsl(var(--accent))',
          foreground: 'hsl(var(--accent-foreground))',
        },
        popover: {
          DEFAULT: 'hsl(var(--popover))',
          foreground: 'hsl(var(--popover-foreground))',
        },
        card: {
          DEFAULT: 'hsl(var(--card))',
          foreground: 'hsl(var(--card-foreground))',
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
