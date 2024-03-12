import {
  Bar,
  BarChart,
  ResponsiveContainer,
  Tooltip,
  XAxis,
  YAxis,
} from 'recharts';
import { trpc } from '~/utils/trpc';

export function Overview() {
  const { data: chartData, isFetched } = trpc.dashboard.chart.useQuery(
    undefined,
    {
      refetchOnWindowFocus: false,
    },
  );
  return (
    <>
      {isFetched && chartData?.data ? (
        <ResponsiveContainer width="100%" height={350}>
          {/* {data} */}
          <BarChart
            data={isFetched && chartData?.data ? (chartData?.data as any) : []}
          >
            <XAxis
              dataKey="name"
              stroke="#888888"
              fontSize={12}
              tickLine={false}
              axisLine={false}
            />
            <YAxis
              stroke="#888888"
              fontSize={12}
              tickLine={false}
              axisLine={false}
              scale="linear"
              tickFormatter={(value) => `AED ${value}`}
            />
            <Tooltip
              cursor={{ fill: 'transparent' }}
              content={<CustomTooltip />}
            />
            <Bar dataKey="total" fill="#23D2B3" radius={[4, 4, 0, 0]} />
            <Bar
              dataKey="discount_amount"
              fill="#aff2e6"
              radius={[4, 4, 0, 0]}
            />
          </BarChart>
        </ResponsiveContainer>
      ) : (
        <></>
      )}
    </>
  );
}
const CustomTooltip = ({ active, payload, label }: any) => {
  if (active && payload && payload.length) {
    return (
      <div className="custom-tooltip w-44 bg-border text-sm rounded-md text-white p-4">
        <div className="  font-bold">Date: {label}</div>
        <p className="label">{`Total Amount : ${+payload[0].value?.toFixed(
          2,
        )}`}</p>
        <p className="label">{`Discount : ${payload[1]?.value}`}</p>
      </div>
    );
  }

  return null;
};
