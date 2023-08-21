import { BannerForm } from '~/components/common/forms/banner';

export default function EditBanner() {
  return (
    <div className="p-8 space-y-8">
      <h2 className="text-4xl font-medium">Edit Banner</h2>
      <BannerForm />
    </div>
  );
}
