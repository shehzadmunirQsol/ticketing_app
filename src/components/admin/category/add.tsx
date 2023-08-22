import CategoryForm from '~/components/common/forms/category';

export default function AddCategory() {
  return (
    <div className="p-8 space-y-8">
      <h2 className="text-4xl font-medium">Add Category</h2>
      <CategoryForm />
    </div>
  );
}
