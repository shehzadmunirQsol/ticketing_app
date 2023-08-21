import {
  AlertDialog,
  AlertDialogContent,
  AlertDialogTrigger,
} from '@/ui/alert-dialog';
import { Button } from '@/ui/button';

export function AlertDialogDemo() {
  return (
    <AlertDialog>
      <AlertDialogTrigger asChild>
        <Button variant="outline">Show Dialog</Button>
      </AlertDialogTrigger>
      <AlertDialogContent>
        <div className="flex flex-col items-center justify-center">
          <div className="loader ease-linear rounded-full border-4 border-t-4 border-gray-200 h-12 w-12 mb-4" />
          <h2 className="text-white text-xl font-semibold">
            Saving the Data...
          </h2>
        </div>
      </AlertDialogContent>
    </AlertDialog>
  );
}
