import CategoryForm from '~/components/common/forms/category';
import { AlertDialogDemo } from '~/components/common/modal/loadingModal';

export default function AddCategory() {
  return (
    <div className="p-8 space-y-8">
      <h2 className="text-4xl font-medium">Add Category</h2>
      <CategoryForm />

      <AlertDialogDemo />

      {/* <div className="fixed top-0 left-0 right-0 bottom-0 w-full h-screen z-50 overflow-hidden bg-black/80 flex flex-col items-center justify-center">
        <div className="loader ease-linear rounded-full border-4 border-t-4 border-gray-200 h-12 w-12 mb-4" />
        <h2 className="text-center text-white text-xl font-semibold">
          Loading...
        </h2>
      </div> */}
    </div>
  );
}
