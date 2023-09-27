import CategoryForm from '~/components/common/forms/categoryForm';
import GlobalBack from '~/components/common/globalBack';

export default function EditCategory() {
  return (
    <div className="p-8 space-y-8">
      <div className="flex items-center justify-between">
        <h2 className="text-4xl font-medium">Edit Category</h2>
        <GlobalBack/>
      </div>
      <CategoryForm />
    </div>
  );
}
