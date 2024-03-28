import { Circle } from 'lucide-react';

interface SponsorProps {
  icon: JSX.Element;
  name: string;
}

const sponsors: SponsorProps[] = [
  {
    icon: <Circle size={34} />,
    name: 'Sponsor 1',
  },
  {
    icon: <Circle size={34} />,
    name: 'Sponsor 2',
  },
  {
    icon: <Circle size={34} />,
    name: 'Sponsor 3',
  },
  {
    icon: <Circle size={34} />,
    name: 'Sponsor 4',
  },
  {
    icon: <Circle size={34} />,
    name: 'Sponsor 5',
  },
  {
    icon: <Circle size={34} />,
    name: 'Sponsor 6',
  },
];

export const Sponsors = () => {
  return (
    <section id="sponsors" className="container pt-24 sm:py-32 mx-auto">
      <h2 className="text-center text-md lg:text-xl font-bold mb-8 text-primary">
        Investors and founders
      </h2>

      <div className="flex flex-wrap justify-center items-center gap-4 md:gap-8">
        {sponsors.map(({ icon, name }: SponsorProps) => (
          <div
            key={name}
            className="flex items-center gap-1 text-muted-foreground/60"
          >
            <>{icon}</>
            <h3 className="text-xl  font-bold">{name}</h3>
          </div>
        ))}
      </div>
    </section>
  );
};
