import * as React from 'react';
import * as ProgressPrimitive from '@radix-ui/react-progress';
import { useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import { cn } from '~/utils/cn';

const Progress = React.forwardRef<
  React.ElementRef<typeof ProgressPrimitive.Root>,
  React.ComponentPropsWithoutRef<typeof ProgressPrimitive.Root>
>(({ className, value, ...props }, ref) => {
  
  const { lang } = useSelector((state: RootState) => state.layout);
  
  var sign = "-";
  if(lang.lang_id===2){
    sign = "+";
  }

  return (
    <ProgressPrimitive.Root
      ref={ref}
      className={cn(
        'relative h-2 w-full overflow-hidden rounded-full bg-zinc-900',
        className,
      )}
      {...props}
    >
      <ProgressPrimitive.Indicator
        className="h-full w-full flex-1 bg-primary rounded-full transition-all"
        style={{ transform: `translateX(${sign}${100 - (value || 0)}%)` }}
      />
    </ProgressPrimitive.Root>
  );
});

Progress.displayName = ProgressPrimitive.Root.displayName;

export { Progress };