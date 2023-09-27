import { BannerForm } from '~/components/common/forms/banner';
import GlobalBack from '~/components/common/globalBack';

export default function EditBanner() {
  return (
    <div className="p-8 space-y-8">
      <div className="flex items-center justify-between">
        <h2 className="text-4xl font-medium">Edit Banner</h2>
        <GlobalBack/>
      </div>
      <BannerForm />
    </div>
  );
}
