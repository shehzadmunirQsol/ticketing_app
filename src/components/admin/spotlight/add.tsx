import { SpotLightForm } from '~/components/common/forms/spotlight';

export default function AddSpotLight() {
  return (
    <div className="p-8 space-y-8">
      <h2 className="text-4xl font-medium">Add Spot Light</h2>
      <SpotLightForm />
    </div>
  );
}
