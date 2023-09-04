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
import { GetCategorySchema } from '~/schema/category';
import { trpc } from '~/utils/trpc';
import Link from 'next/link';
import { GetEventSchema } from '~/schema/event';
import { ScrollArea, ScrollBar } from '~/components/ui/scroll-area';

export type Category = {
  id: number;
  email: string;
  first_name: string;
  last_name: string;
  phone_number: string;
  total_amount: number;
  discount_amount: number;
  sub_total_amount: number;
  total_payment_id: string;

  created_at: Date;
  updated_at: Date;
};

export default function OrdersDataTable() {
  const [sorting, setSorting] = useState<SortingState>([]);
  const [filters, setFilters] = useState<GetEventSchema>({
    first: 0,
    rows: 10,
    lang_id: 1,
  });
  const [columnVisibility, setColumnVisibility] = useState<VisibilityState>({});
  const [rowSelection, setRowSelection] = useState({});

  const { data } = trpc.order.get.useQuery(filters, {
    refetchOnWindowFocus: false,
  });
  console.log({ data });
  const categoryData = React.useMemo(() => {
    return Array.isArray(data?.data) ? data?.data : [];
  }, [data]);
  const columns: ColumnDef<Category>[] = [
    {
      accessorKey: 'name',
      header: 'Name',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap ">
          {row?.original?.first_name + ' ' + row?.original?.last_name}
        </div>
      ),
    },
    {
      accessorKey: 'email',
      header: 'Email',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap ">
          {row?.original?.email}
        </div>
      ),
    },

    {
      accessorKey: 'phone_number',
      header: 'Phone No.',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap ">
          {row?.original?.phone_number}
        </div>
      ),
    },
    {
      accessorKey: 'total_payment_id',
      header: 'Transaction ID',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap ">
          {row?.original?.total_payment_id}
        </div>
      ),
    },
    {
      accessorKey: 'sub_total_amount',
      header: 'Sub Total',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap ">
          AED {(row?.original?.sub_total_amount).toFixed(2)}
        </div>
      ),
    },
    {
      accessorKey: 'discount_amount',
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
      accessorKey: 'total_amount',
      header: 'Total Amount',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap ">
          {' '}
          {row?.original?.total_amount > 0
            ? 'AED ' + (row?.original?.total_amount).toFixed(2)
            : 'N/A'}
        </div>
      ),
    },

    {
      id: 'actions',
      enableHiding: false,
      cell: ({ row }) => {
        return (
          <DropdownMenu>
            <DropdownMenuTrigger asChild>
              <Button variant="ghost" className="h-8 w-8 p-0">
                <span className="sr-only">Open menu</span>
                <MoreHorizontal className="h-4 w-4" />
              </Button>
            </DropdownMenuTrigger>
            <DropdownMenuContent align="end">
              <DropdownMenuLabel>Actions</DropdownMenuLabel>
              <DropdownMenuSeparator />
              <Link href={`/admin/orders/view/${row?.original?.id}`}>
                <DropdownMenuItem>View Order</DropdownMenuItem>
              </Link>
            </DropdownMenuContent>
          </DropdownMenu>
        );
      },
    },
  ];
  const table = useReactTable({
    data: categoryData as Category[],
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
    setFilters((prevFilters) => ({ ...prevFilters }));
  }

  function handlePagination(page: number) {
    if (page < 0) return;
    setFilters((prevFilters) => ({ ...prevFilters, first: page }));
  }

  return (
    <div className="w-full space-y-4">
      <div className="flex items-center justify-end gap-2">
        <DropdownMenu>
          <DropdownMenuTrigger asChild>
            <Button variant="outline">
              Columns <ChevronDown className="ml-2 h-4 w-4" />
            </Button>
          </DropdownMenuTrigger>
          <DropdownMenuContent align="end">
            {table
              .getAllColumns()
              .filter((column) => column.getCanHide())
              .map((column) => {
                return (
                  <DropdownMenuCheckboxItem
                    key={column.id}
                    className="capitalize"
                    checked={column.getIsVisible()}
                    onCheckedChange={(value) =>
                      column.toggleVisibility(!!value)
                    }
                  >
                    {column.id}
                  </DropdownMenuCheckboxItem>
                );
              })}
          </DropdownMenuContent>
        </DropdownMenu>
      </div>
      <div className="rounded-md border">
        <ScrollArea className="w-full ">
          <ScrollBar orientation="horizontal"></ScrollBar>
          <Table>
            <TableHeader>
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
            onClick={() => handlePagination(filters.first - 1)}
            disabled={filters.first === 0}
          >
            Previous
          </Button>
          <Button
            variant="outline"
            onClick={() => handlePagination(filters.first + 1)}
            disabled={(filters.first + 1) * filters.rows > (data?.count || 0)}
          >
            Next
          </Button>
        </div>
      </div>
    </div>
  );
}
