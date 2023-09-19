import React, { useState } from 'react';
import {
  ColumnDef,
  // ColumnFiltersState,
  SortingState,
  VisibilityState,
  flexRender,
  getCoreRowModel,
  getFilteredRowModel,
  getPaginationRowModel,
  getSortedRowModel,
  useReactTable,
} from '@tanstack/react-table';
import { ChevronDown, MoreHorizontal } from 'lucide-react';

import { Button } from '@/ui/button';
import {
  DropdownMenu,
  DropdownMenuCheckboxItem,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuLabel,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
} from '@/ui/dropdown-menu';
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from '@/ui/table';
import LanguageSelect, { LanguageInterface } from '../language_select';
import { trpc } from '~/utils/trpc';
import Link from 'next/link';
import { GetEventSchema } from '~/schema/event';
import { ScrollArea, ScrollBar } from '~/components/ui/scroll-area';
import { LoadingDialog } from '../modal/loadingModal';
import { TableFilters } from './table_filters';
import { displayDate } from '~/utils/helper';
import { getOrder } from '~/schema/order';

export type Category = {
  id: number;
  total_amount: number;
  discount_amount: number;
  sub_total_amount: number;

  created_at: Date;
};

interface OrderTableProps {
  filters: any;
  setFilters: any;
}

export default function OrdersDataByIdTable(props: OrderTableProps) {
  const [sorting, setSorting] = useState<SortingState>([]);
  //   const { customer_id, status, ...filterData } = { ...props.filters };
  //   const [filters, setFilters] = useState<getOrder>({
  //     ...filterData,
  //   });
  const [columnVisibility, setColumnVisibility] = useState<VisibilityState>({});
  const [rowSelection, setRowSelection] = useState({});
  const [filterID, setFilterID] = useState({});

  const { data, isLoading } = trpc.order.getOrders.useQuery(
    { ...props.filters },
    {
      refetchOnWindowFocus: false,
      enabled: props?.filters.customer_id ? true : false,
    },
  );

  const orderData = React.useMemo(() => {
    return Array.isArray(data?.data) ? data?.data : [];
  }, [data]);

  const columns: ColumnDef<Category>[] = [
    {
      accessorKey: 'ID',
      header: 'Order ID',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap ">
          #{row?.original.id}
        </div>
      ),
    },
    {
      accessorKey: 'Sub Total',
      header: 'Sub Total',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap ">
          AED {(row?.original?.sub_total_amount).toFixed(2)}
        </div>
      ),
    },
    {
      accessorKey: 'Discount',
      header: 'Discount',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap ">
          {' '}
          {row?.original?.discount_amount > 0
            ? 'AED ' + (row?.original?.discount_amount).toFixed(2)
            : 'N/A'}
        </div>
      ),
    },
    {
      accessorKey: 'Total Amount',
      header: 'Total Amount',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap w-28">
          {' '}
          {row?.original?.total_amount > 0
            ? 'AED ' + (row?.original?.total_amount).toFixed(2)
            : 'N/A'}
        </div>
      ),
    },
    {
      accessorKey: 'Created At',
      header: 'Date',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap overflow-hidden ">
          {displayDate(row?.original?.created_at)}
        </div>
      ),
    },
  ];
  const table = useReactTable({
    data: orderData as Category[],
    columns,
    onSortingChange: setSorting,
    getCoreRowModel: getCoreRowModel(),
    getPaginationRowModel: getPaginationRowModel(),
    getSortedRowModel: getSortedRowModel(),
    getFilteredRowModel: getFilteredRowModel(),
    onColumnVisibilityChange: setColumnVisibility,
    onRowSelectionChange: setRowSelection,
    state: {
      sorting,
      columnVisibility,
      rowSelection,
    },
  });

  function languageHandler(params: LanguageInterface) {
    props?.setFilters((prevFilters: any) => ({ ...prevFilters }));
  }

  function handlePagination(page: number) {
    if (page < 0) return;
    props?.setFilters((prevFilters: any) => ({ ...prevFilters, first: page }));
  }
  const roleOptions1 = [
    {
      Icon: 'fal fa-chevron-down',
      text: 'Search',
      filtername: 'searchQuery',
      type: 'text',
    },

    {
      Icon: 'fal fa-chevron-down',
      text: 'From Date',
      filtername: 'startDate',
      type: 'date',
    },
    {
      Icon: 'fal fa-chevron-down',
      text: 'To Date',
      filtername: 'endDate',
      type: 'date',
    },
    {
      Icon: 'fal fa-chevron-down',
      text: 'Clear Filter',
      filtername: 'Clear',
    },
  ];
  console.log(
    (props.filters.first + 1) * props.filters.rows > (data?.count || 0),
    'next page',
  );
  return (
    <div className="w-full p-2">
      <div className="rounded-md border border-border">
        <ScrollArea className="w-full ">
          <ScrollBar orientation="horizontal"></ScrollBar>
          <Table>
            <TableHeader className="bg-secondary/80">
              {table?.getHeaderGroups().map((headerGroup) => (
                <TableRow key={headerGroup.id}>
                  {headerGroup.headers.map((header) => {
                    return (
                      <TableHead key={header.id}>
                        {header.isPlaceholder
                          ? null
                          : flexRender(
                              header.column.columnDef.header,
                              header.getContext(),
                            )}
                      </TableHead>
                    );
                  })}
                </TableRow>
              ))}
            </TableHeader>
            <TableBody>
              {table?.getRowModel()?.rows?.length ? (
                table?.getRowModel()?.rows?.map((row) => (
                  <TableRow
                    key={row.id}
                    data-state={row.getIsSelected() && 'selected'}
                  >
                    {row.getVisibleCells().map((cell) => (
                      <TableCell key={cell.id}>
                        {flexRender(
                          cell.column.columnDef.cell,
                          cell.getContext(),
                        )}
                      </TableCell>
                    ))}
                  </TableRow>
                ))
              ) : (
                <TableRow>
                  <TableCell
                    colSpan={columns.length}
                    className="h-24 text-center"
                  >
                    No results.
                  </TableCell>
                </TableRow>
              )}
            </TableBody>
          </Table>
        </ScrollArea>
      </div>

      <div className="flex items-center justify-end space-x-2 py-4">
        <div className="space-x-2">
          <Button
            variant="outline"
            onClick={() => handlePagination(props.filters.first - 1)}
            disabled={props.filters.first === 0}
          >
            Previous
          </Button>
          <Button
            variant="outline"
            onClick={() => handlePagination(props.filters.first + 1)}
            disabled={
              (props.filters.first + 1) * props.filters.rows >
              (data?.count || 0)
            }
          >
            Next
          </Button>
        </div>
      </div>
      <LoadingDialog open={isLoading} text={'Loading data...'} />
    </div>
  );
}
