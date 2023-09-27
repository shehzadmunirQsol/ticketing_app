import { SpotLightForm } from '~/components/common/forms/spotlight';
import GlobalBack from '~/components/common/globalBack';

export default function AddSpotLight() {
  return (
    <div className="p-8 space-y-8">
      <div className="flex items-center justify-between">
        <h2 className="text-4xl font-medium">Add Spotlight</h2>
        <GlobalBack/>
      </div>
      <SpotLightForm />
    </div>
  );
}
